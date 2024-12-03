library(ffsimulator)
library(tidyverse)

my_league <- ff_connect("sleeper", "1121217308480954368", season = 2024)
my_league

my_league <- ffscrapr::ff_connect("sleeper", "1045662785243389952", season = 2024) # unbound keeper
my_league

my_sim <- ff_simulate(
  my_league, seed = 1234, actual_schedule = TRUE, n_seasons = 1000, pos_filter = c('QB', 'RB', 'WR', 'TE', 'K'), n_weeks = 16
)

#plot(my_sim)
PLAYOFF_SIZE <- if(ffscrapr::ff_league(my_league) |> pull(league_name) == 'Unbound Keeper') 4 else 5
message(PLAYOFF_SIZE)
SIMS <- 1000

standings <- ffscrapr::ff_standings(my_league) |> mutate(franchise_id = as.character(franchise_id))

my_sim$summary_season |>
  left_join(standings, by = c('franchise_id', 'franchise_name')) |>
  as_tibble() |>
  mutate(
    season_wins = h2h_wins.x + h2h_wins.y,
    pf = points_for.x + points_for.y
  ) |>
  group_by(season) |>
  arrange(-season_wins, -pf) |>
  mutate(standing = row_number()) |>
  ungroup() -> simulated_seasons

simulated_seasons |>
  group_by(franchise_id, franchise_name) |>
  summarise(
    sim_wins = mean(season_wins),
    sim_pf = mean(pf),
    playoffs = sum(standing <= 4) / max(season)
  ) |>
  arrange(-sim_wins, -sim_pf)

simulated_seasons



source('helper_plot.R')


playoff_table <- simulated_seasons |>
  group_by(franchise_id, franchise_name) |>
  summarise(
    wins = (sum(h2h_wins.x) + sum(h2h_wins.y)),
    pf = sum(points_for.x) + sum(points_for.y),
    pa = sum(points_against.x + points_against.y),
    playoffs = sum(standing <= PLAYOFF_SIZE)) |>
  ungroup() |>
  mutate(across(where(is.numeric), ~. / SIMS)) |>
  arrange(-wins, -pf) |>
  left_join(
simulated_seasons |> count(franchise_id, franchise_name, standing) |>
  pivot_wider(id_cols = starts_with('franchise'), names_from = standing, names_sort = TRUE, values_from = n, values_fn = ~./SIMS, values_fill = 0)

  ) |>
  mutate(
    rank = row_number(),
    wins = round(wins, 2),
  ) |>
  relocate(rank, .before = franchise_id) |>
  select(-franchise_id)
playoff_table

reactable(
  playoff_table,
  theme = reactablefmtr::fivethirtyeight(),
  highlight = TRUE,
  defaultPageSize = 12,
  columnGroups = list(
    # colGroup(name = "Points", columns = c('pf', 'pa')),
    #colGroup(name = "Regular Season", columns = c('bye', 'last')),
    colGroup(name = "Standings", columns = c('1', '2', '3','4', '5','6','7', '8', '9', '10'))
  ),
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
    # power_rank = colDef(
    #   name = "Team Strength",
    #   maxWidth = 80,
    #   # cell = pill_buttons(
    #   #   df3,
    #   #   colors = c('lightpink', '#f8fcf8', 'lightgreen')
    #   # )
    #   cell = icon_assign(playoff_table, icon = "fire", fill_color = "orangered", empty_color = "lightgrey", buckets = 5)
    # ),
    wins = colDef(
      # align = 'left',
      align = "center",
      name = "Sim Wins",
      maxWidth = 70,
      # maxWidth = 150,
      # cell = icon_assign(df3, icon = 'trophy', fill_color = 'goldenrod', empty_opacity = 0)
    ),
    pf = rating_column(
      name = "Sim PF",
      style = function(value) {
        scaled <- (value - min(playoff_table$pf)) / (max(playoff_table$pf) - min(playoff_table$pf))
        color <- off_rating_color(scaled)
        value <- format(round(value))
        list(background = color)
      }
    ),
    pa = rating_column(
      name = "Sim PA",
      style = function(value) {
        scaled <- (value - min(playoff_table$pa)) / (max(playoff_table$pa) - min(playoff_table$pa))
        color <- def_rating_color(scaled)
        value <- format(round(value))
        list(background = color)
      }
    ),
    playoffs = playoff_column(name = "Playoffs", class = "border-left", borderLeft = '2px solid #000000', , maxWidth = 50),
    `1` = knockout_column(class = "border-left", maxWidth = 50),
    `2` = knockout_column(class = "border-left", maxWidth = 50),
    `3` = knockout_column(class = "border-left", maxWidth = 50),
    `4` = knockout_column(class = "border-left", maxWidth = 50),
    `5` = knockout_column(class = "border-left", maxWidth = 50),
    `6` = knockout_column(class = "border-left", maxWidth = 50),
    `7` = knockout_column(class = "border-left", maxWidth = 50),
    `8` = knockout_column(class = "border-left", maxWidth = 50),
    `9` = knockout_column(class = "border-left", maxWidth = 50),
    `10` = knockout_column(class = "border-left", maxWidth = 50)

  )
)

simulated_seasons |>
  count(franchise_id, franchise_name)

simulated_seasons |> count(franchise_id, franchise_name, standing) |>
  filter(franchise_id %in% c(1, 3)) |>
  mutate(nn = n * standing %in% c(2, 3)) |>
  summarise(x = sum(nn)) |>
  mutate(x = x/(SIMS*2))
