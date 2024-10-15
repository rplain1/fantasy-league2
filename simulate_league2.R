library(tidyverse)
library(ffscrapr)
library(tidybayes)
library(duckdb)
library(brms)

source('helper_plot.R')

my_league <- ff_connect("sleeper", "1121217308480954368", season = 2024)
my_league

my_league <- ff_connect("sleeper", "1045662785243389952", season = 2024) # unbound keeper
my_league


set.seed(527)

WEEK = 6
SIMS = 10000
PLAYOFF_SIZE = 4

df <- ff_schedule(my_league)
franchises <- ff_franchises(my_league)
standings <- ff_standings(my_league)

df_input <- df |>
  filter(week <= 6) |>
  left_join(franchises)

mean_scores = mean(df_input$franchise_score)
sd_scores = sd(df_input$franchise_score)
fit <- brm(
  franchise_score ~ 1 + (1 | franchise_name),
  data = df_input,
  prior = c(
    prior(normal(120, 20), class = Intercept),
    prior(exponential(0.3), class = sd),
    prior(exponential(0.3), class = sigma)
  ),
  iter = 4000,
  warmup = 1500,
  chains = 4,
  cores = 4,
  seed = 2
)

con <- dbConnect(duckdb(), dbdir = ":memory:")
dbWriteTable(con, "scores", franchises |>
  add_epred_draws(fit) |>
  ungroup(),
overwrite = TRUE
)
rm(fit)
gc()

dbWriteTable(con, "schedule", df, overwrite = TRUE)


tbl(con, "scores") |>
  group_by(franchise_id, franchise_name) |>
  summarise(scores = list(.epred)) |>
  compute(name = "scores_nested", overwrite = TRUE)


tbl(con, "schedule") |>
  left_join(tbl(con, "scores_nested")) |>
  mutate(
    scores = sql("unnest(scores)"),
  ) |>
  group_by(franchise_id, week) |>
  mutate(sim_id = row_number()) |>
  ungroup() |>
  compute(name = "simulated_scores", overwrite = TRUE)


tbl(con, "simulated_scores") |>
  left_join(
    tbl(con, "simulated_scores") |>
      select(week, franchise_id, sim_id, scores_opp = scores),
    by = c("opponent_id" = "franchise_id", "sim_id", "week")
  ) |>
  mutate(
    win = ifelse(week <= WEEK, as.numeric(franchise_score > opponent_score), as.numeric(scores > scores_opp)),
    pf = ifelse(week <= WEEK, franchise_score, scores),
    pa = ifelse(week <= WEEK, opponent_score, scores_opp)
  ) |>
  compute(name = "simulated_season", overwrite = TRUE)

standings <- tbl(con, "simulated_season") |>
  group_by(franchise_id, franchise_name) |>
  summarise(
    wins = sum(win) / SIMS,
    pf = sum(pf) / SIMS
  ) |>
  collect() |>
  arrange(-wins, -pf)

tbl(con, "simulated_season") |>
  group_by(sim_id, franchise_id) |>
  summarise(wins = sum(win), pf = sum(pf), .groups = "drop") |>
  mutate(
    standing = sql("ROW_NUMBER() OVER (PARTITION BY sim_id ORDER BY wins DESC, pf DESC)")
  ) |>
  compute(name = "simulated_standings", overwrite = TRUE)


#### PLAYOFF SIM
if(PLAYOFF_SIZE == 5) {
  tbl(con, "simulated_standings") |>
    filter(standing <= 5) |>
    select(sim_id, franchise_id, standing) |>
    compute(name = "tmp", overwrite = TRUE)

  tbl(con, "tmp") |>
    mutate(join_id = case_when(
      standing == 5 ~ 4,
      standing == 4 ~ 5,
      TRUE ~ NA
    )) |>
    filter(!is.na(join_id)) |>
    compute("tmp2", overwrite = TRUE)

  dbWriteTable(
    con,
    "w_15_scores",
    tbl(con, "tmp2") |>
      left_join(tbl(con, "tmp"), by = c("join_id" = "standing", "sim_id")) |>
      left_join(
        tbl(con, "scores_nested"),
        by = c("franchise_id.x" = "franchise_id"), # '.id')
      ) |>
      collect() |>
      mutate(score = map_dbl(.x = scores, .f = ~ sample(.x, 1))) |>
      select(sim_id, franchise_id.x, franchise_id.y, score),
    overwrite = TRUE
  )

  tbl(con, "w_15_scores") |>
    left_join(tbl(con, "w_15_scores"), by = c("franchise_id.x" = "franchise_id.y", "sim_id")) |>
    mutate(
      w_15_result = score.x > score.y
    ) |>
    filter(w_15_result) |>
    select(sim_id, franchise_id = franchise_id.x, w_15_result) |>
    compute(name = "w_15_winners", overwrite = TRUE)

  tbl(con, "tmp") |>
    left_join(tbl(con, "w_15_winners")) |>
    filter(w_15_result | standing <= 3) |>
    group_by(sim_id) |>
    mutate(
      join_id = case_when(
        standing == 1 ~ max(standing),
        standing == 2 ~ 3,
        standing == 3 ~ 2,
        standing == max(standing) ~ 1
      )
    ) |>
    ungroup() |>
    compute("w_16_teams", overwrite = TRUE)


} else if (PLAYOFF_SIZE == 4) {

  tbl(con, "simulated_standings") |>
    filter(standing <= 4) |>
    select(sim_id, franchise_id, standing) |>
    compute(name = "tmp", overwrite = TRUE)


  tbl(con, "tmp") |>
  group_by(sim_id) |>
  mutate(
    join_id = case_when(
      standing == 1 ~4,
      standing == 2 ~ 3,
      standing == 3 ~ 2,
      standing == 4 ~ 1
    )
  ) |>
  ungroup() |>
  compute("w_16_teams", overwrite = TRUE)

}


dbWriteTable(
  con,
  "w_16_scores",
  tbl(con, "w_16_teams") |>
    left_join(tbl(con, "tmp"), by = c("join_id" = "standing", "sim_id")) |>
    left_join(
      tbl(con, "scores_nested"),
      by = c("franchise_id.x" = "franchise_id"), # '.id')
    ) |>
    collect() |>
    mutate(score = map_dbl(.x = scores, .f = ~ sample(.x, 1))) |>
    select(sim_id, franchise_id.x, franchise_id.y, score),
  overwrite = TRUE
)


tbl(con, "w_16_scores") |>
  left_join(tbl(con, "w_16_scores"), by = c("franchise_id.x" = "franchise_id.y", "sim_id")) |>
  mutate(
    w_16_result = score.x > score.y
  ) |>
  filter(w_16_result) |>
  select(sim_id, franchise_id = franchise_id.x, w_16_result) |>
  compute(name = "w_16_winners", overwrite = TRUE)


dbWriteTable(
  con,
  "finals_scores",
  tbl(con, "tmp") |>
    inner_join(tbl(con, "w_16_winners"), by = c("sim_id", "franchise_id")) |>
    left_join(tbl(con, "scores_nested")) |>
    collect() |>
    mutate(score = map_dbl(.x = scores, .f = ~ sample(.x, 1))) |>
    select(sim_id, franchise_id, score),
  overwrite = TRUE
)




tbl(con, "finals_scores") |>
  group_by(sim_id) |>
  filter(score == max(score)) |>
  ungroup() |>
  compute(name = "finals_winner", overwrite = TRUE)


standings <- tbl(con, "simulated_season") |>
  group_by(franchise_id, franchise_name) |>
  summarise(
    sim_wins = sum(win) / SIMS,
    sim_pf = sum(pf) / SIMS
  ) |>
  collect() |>
  arrange(-sim_wins, -sim_pf)

playoffs <- tbl(con, "tmp") |>
  count(franchise_id) |>
  collect() |>
  mutate(playoffs = n / SIMS) |>
  select(-n)

bye <- tbl(con, "simulated_standings") |>
  filter(standing <= 3) |>
  count(franchise_id) |>
  collect() |>
  mutate(bye = n / SIMS) |>
  select(-n)

if(PLAYOFF_SIZE == 5) {
round1 <- tbl(con, "tmp") |>
  left_join(tbl(con, "w_15_winners")) |>
  filter(standing <= 3 | w_15_result) |>
  collect() |>
  count(franchise_id) |>
  mutate(semi_finals = n / SIMS) |>
  select(-n)

} else {
  round1 <- tbl(con, "tmp") |> count(franchise_id) |> select(-n) |> collect()
}

round2 <- tbl(con, "w_16_winners") |>
  count(franchise_id) |>
  collect() |>
  mutate(finals = n / SIMS) |>
  select(-n)

winner <- tbl(con, "finals_winner") |>
  count(franchise_id) |>
  collect() |>
  mutate(winner = n / SIMS) |>
  select(-n)

power_rank <- tbl(con, "scores") |>
  group_by(franchise_id) |>
  summarise(x = median(.epred)) |>
  collect() |>
  arrange(-x) |>
  mutate(power_rank = round(pnorm((x - mean(x)) / sd(x)) * 5, 1))

current_standings <- tbl(con, "schedule") |>
  filter(week <= WEEK) |>
  collect() |>
  group_by(franchise_id) |>
  summarise(
    wins = sum(franchise_score > opponent_score),
    pf = sum(franchise_score),
    pa = sum(opponent_score)
  ) |>
  arrange(-wins, -pf) |>
  mutate(rank = row_number())

playoff_table <- current_standings |>
  left_join(standings) |>
  left_join(power_rank) |>
  left_join(playoffs) |>
  left_join(bye) |>
  left_join(round1) |>
  left_join(round2) |>
  left_join(winner) |>
  relocate(rank, .before = wins) |>
  relocate(franchise_name, .after = rank) |>
  relocate(power_rank, .after = wins) |>
  relocate(playoffs, .after = bye) |>
  mutate(
    wins = paste0(wins, "-", 6 - wins)
  ) |>
  select(-franchise_id, -x, -starts_with("sim_")) |>
  replace_na(list(bye = 0, playoffs = 0, finals = 0, winner = 0))



playoff_table

reactable(
  playoff_table,
  theme = reactablefmtr::fivethirtyeight(),
  highlight = TRUE,
  defaultPageSize = 12,
  # columnGroups = list(
  #   # colGroup(name = "Points", columns = c('pf', 'pa')),
  #   #colGroup(name = "Regular Season", columns = c('bye', 'last')),
  #   colGroup(name = "Postseason Projections", columns = c('last', 'bye', 'playoffs', 'semi_finals', 'finals', 'winner'))
  # ),
  columns = list(
    rank = colDef(
      width = 60,
      align = "center",
      name = "Rank",
      style = list(borderRight = "2px solid #777")
    ),
    franchise_name = colDef(
      # show = FALSE,
      name = "Team",
      maxWidth = 200
    ),
    power_rank = colDef(
      name = "Team Strength",
      maxWidth = 80,
      # cell = pill_buttons(
      #   df3,
      #   colors = c('lightpink', '#f8fcf8', 'lightgreen')
      # )
      cell = icon_assign(playoff_table, icon = "fire", fill_color = "orangered", empty_color = "lightgrey", buckets = 5)
    ),
    wins = colDef(
      # align = 'left',
      align = "center",
      name = "Record",
      maxWidth = 70,
      # maxWidth = 150,
      # cell = icon_assign(df3, icon = 'trophy', fill_color = 'goldenrod', empty_opacity = 0)
    ),
    pf = rating_column(
      name = "PF",
      style = function(value) {
        scaled <- (value - min(playoff_table$pf)) / (max(playoff_table$pf) - min(playoff_table$pf))
        color <- off_rating_color(scaled)
        value <- format(round(value))
        list(background = color)
      }
    ),
    pa = rating_column(
      name = "PA",
      style = function(value) {
        scaled <- (value - min(playoff_table$pa)) / (max(playoff_table$pa) - min(playoff_table$pa))
        color <- def_rating_color(scaled)
        value <- format(round(value))
        list(background = color)
      }
    ),
    bye = loser_column(
      show = FALSE,
      name = "1st Round Bye", class = "border-left", borderRight = "2px solid #000000"),
    # loser_bracket = colDef(show = FALSE),
    # loser_bracket = loser_column(name = 'Losers Bracket', class = 'border-left'),
    # last_place = colDef(show = FALSE),
    #bye = loser_column(name = 'Last Place', class = 'border-left', borderRight = '2px solid #000000'),
    playoffs = playoff_column(name = "Playoff", class = "border-left", borderLeft = '2px solid #000000'),
    #semi_finals = knockout_column(name = "Semi-Finals", class = "border-left"),
    finals = knockout_column(name = "Finals", class = "border-left"),
    winner = knockout_column(name = "Winner", class = "border-left")
  )
)
