library(tidyverse)


get_scores_dist <- function(fantasy_schedule, franchises, week_num) {

  fantasy_schedule <- filter(fantasy_schedule, week <= week_num)

  fantasy_schedule |>
    rowwise() |>
    mutate(un = list(1:week)) |>
    unnest(un) |>
    bind_rows(
      fantasy_schedule |>
        rowwise() |>
        mutate(un2 = list(1:length(unique(fantasy_schedule$franchise_id)))) |>
        unnest(un2) |>
        mutate(franchise_id = un2) |>
        select(-un2)
    ) |>
    left_join(franchises, by='franchise_id') |>
    select(franchise_id, franchise_name,franchise_score) |>
    group_by(franchise_id, franchise_name) |>
    nest(data = franchise_score) |>
    ungroup() |>
    rowwise() |>
    mutate(dens = list(density(data$franchise_score))) |>
    ungroup()


}


simulate_season <- function(fantasy_schedule, franchises, week_num, season_end = 14, sims = 1e5, win_franchise_id = FALSE) {

  df_scores <- get_scores_dist(fantasy_schedule, franchises, week_num = week_num)
  df_schedule <- fantasy_schedule

  #message('Simulating season')

  df_franchise_scores <- df_schedule |>
    mutate(
      id = list(1:sims)
    ) |>
    unnest(id) |>
    arrange(id, week, franchise_id) |>
    filter(week <= 14) |>
    left_join(df_scores, by = c('franchise_id')) |>
    select(-opponent_score, -result, -data) |>
    mutate(
      f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
      franchise_score = ifelse(week > week_num, f_score, franchise_score)
    ) |>
    select(week, ends_with('id'), franchise_score, franchise_name)

  df_joined_franchises <- df_franchise_scores  |>
    left_join(df_franchise_scores, by = c('franchise_id' = 'opponent_id', 'week', 'id'), suffix = c('_f', '_o'))

  if(win_franchise_id) {
    adjust_week_num <- week_num + 1
    message(glue::glue("Setting week {adjust_week_num} win: Franchise {win_franchise_id}"))

    df_joined_franchises <- df_joined_franchises |>
      mutate(
        franchise_score_f = case_when(
          week == adjust_week_num & franchise_id == win_franchise_id ~ franchise_score_o + 1,
          week == adjust_week_num & opponent_id == win_franchise_id ~ franchise_score_o - 1,
          TRUE ~ franchise_score_f
        )
      )
      # mutate(
      #   franchise_score_f_tmp = ifelse(
      #     franchise_score_f < franchise_score_o &
      #       week == adjust_week_num &
      #       (franchise_id == win_franchise_id | opponent_id == win_franchise_id),
      #     franchise_score_o,
      #     franchise_score_f
      #     ),
      #   franchise_score_o_tmp = ifelse(
      #     franchise_score_f > franchise_score_o &
      #       week == adjust_week_num &
      #       (franchise_id == win_franchise_id | opponent_id == win_franchise_id),
      #     franchise_score_f,
      #     franchise_score_o
      #     )
      #   ) |>
      # select(-franchise_score_o, -franchise_score_f) |>
      # rename(franchise_score_f = franchise_score_f_tmp, franchise_score_o = franchise_score_o_tmp)
  }


  df_joined_franchises |>
    mutate(result = franchise_score_f > franchise_score_o)

}
#
# franchises <- ffscrapr::ff_franchises(my_league)
# df_sim <- simulate_season(df, franchises, 10, 14, 1e4)
# simulate_playoffs(df_sim, df, 14)
#




simulate_playoffs <- function(df_season_sim, fantasy_schedule, week_num, playoff_size = 6) {

  #df_scores <- get_scores_dist(league_object, week_num)
  df_scores <- get_scores_dist(fantasy_schedule, franchises, week_num = week_num)
  df_sim <- df_season_sim |>
    group_by(id, franchise_id) |>
    summarise(
      w = sum(result),
      pf = sum(franchise_score_f)
    ) |>
    arrange(-w, -pf) |>
    mutate(
      rank = row_number()
    ) |>
    ungroup()

  sims <- max(df_season_sim$id)

  if(playoff_size == 6) {

    tmp <- df_sim |>
      filter(rank <= 6) |>
      select(id, franchise_id, rank)


    tmp_week_15 <- tmp |>
      mutate(join_id = case_when(
        rank == 3 ~ 6,
        rank == 4 ~ 5,
        rank == 5 ~ 4,
        rank == 6 ~ 3,
        TRUE ~ NA
      )) |>
      filter(!is.na(join_id)) |>
      left_join(tmp, by = c('join_id' = 'rank', 'id')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id.x, franchise_id.y, f_score)


    w15_winners <- tmp_week_15 |>
      left_join(tmp_week_15, by=c('id', 'franchise_id.x' = 'franchise_id.y')) |>
      mutate(
        w15_result = f_score.x > f_score.y
      ) |>
      filter(w15_result) |>
      select(id, franchise_id = franchise_id.x) |>
      mutate(result = TRUE)


    w16_teams <- tmp |>
      left_join(w15_winners, by = c('id', 'franchise_id')) |>
      filter(result | rank <= 2) |>
      group_by(id) |>
      mutate(
        join_id = case_when(
          rank == min(ifelse(rank <= 2, NA, rank), na.rm = TRUE) ~ 1,
          rank == max(ifelse(rank <= 2, NA, rank), na.rm = TRUE) ~ 2,
          rank == 1 ~ min(ifelse(rank <= 2, NA, rank), na.rm = TRUE),
          rank == 2 ~ max(rank),
          TRUE ~ NA
        )
      ) |>
      ungroup()


    tmp_week_16 <- w16_teams |>
      #filter(!is.na(join_id)) |>
      left_join(tmp |> mutate(rank2 = rank), by = c('id','join_id' = 'rank')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id = franchise_id.x, opponent_id = franchise_id.y, f_score)


    w16_winners <- tmp_week_16 |>
      left_join(tmp_week_16, by=c('id', 'opponent_id' = 'franchise_id')) |>
      mutate(
        w16_result = f_score.x > f_score.y
      ) |>
      filter(w16_result) |>
      select(id, franchise_id) |>
      mutate(result = TRUE)



    finals_teams <- tmp |>
      left_join(w16_winners, by = c('id', 'franchise_id')) |>
      filter(result) |>
      left_join(df_scores, by = c('franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id, f_score)


    finals_winner <- finals_teams |>
      group_by(id) |>
      filter(f_score == max(f_score)) |>
      ungroup()

    round1 <- tmp |>
      left_join(w15_winners) |>
      filter(rank <= 2 | result) |>
      count(franchise_id) |>
      mutate(semi_finals = n/sims) |>
      select(-n)

    round2 <- w16_winners |>
      count(franchise_id) |>
      mutate(finals = n/sims) |>
      select(-n)

    winner <- finals_winner |>
      count(franchise_id) |>
      mutate(winner = n/sims) |>
      select(-n)

    return(
    round1 |>
      left_join(round2) |>
      left_join(winner)
    )

  }

  else if (playoff_size == 4) {
    tmp <- df_sim |>
      filter(rank <= 4) |>
      select(id, franchise_id, rank)


    tmp_week_15 <- tmp |>
      mutate(join_id = case_when(
        rank == 1 ~ 4,
        rank == 4 ~ 1,
        rank == 2 ~ 3,
        rank == 3 ~ 2,
        TRUE ~ NA
      )) |>
      filter(!is.na(join_id)) |>
      left_join(tmp, by = c('join_id' = 'rank', 'id')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id.x, franchise_id.y, f_score)


    w15_winners <- tmp_week_15 |>
      left_join(tmp_week_15, by=c('id', 'franchise_id.x' = 'franchise_id.y')) |>
      mutate(
        w15_result = f_score.x > f_score.y
      ) |>
      filter(w15_result) |>
      select(id, franchise_id = franchise_id.x) |>
      mutate(result = TRUE)


    w16_teams <- tmp |>
      left_join(w15_winners, by = c('id', 'franchise_id')) |>
      filter(result) |>
      group_by(id) |>
      mutate(
        join_id = case_when(
          rank == min(rank) ~ max(rank),
          rank == max(rank) ~ min(rank),
          TRUE ~ NA
        )
      ) |>
      ungroup()


    tmp_week_16 <- w16_teams |>
      #filter(!is.na(join_id)) |>
      left_join(tmp, by = c('id','join_id' = 'rank')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id = franchise_id.x, opponent_id = franchise_id.y, f_score)


    w16_winners <- tmp_week_16 |>
      left_join(tmp_week_16, by=c('id', 'opponent_id' = 'franchise_id')) |>
      mutate(
        w16_result = f_score.x > f_score.y
      ) |>
      filter(w16_result) |>
      select(id, franchise_id) |>
      mutate(result = TRUE)

    w15_results <- w15_winners |>
      count(franchise_id) |>
      mutate(finals = n/sims) |>
      select(-n)

    w16_results <- w16_winners |>
      count(franchise_id) |>
      mutate(winner = n / sims) |>
      select(-n)

    return(w15_results |> left_join(w16_results))

  }

  else if (playoff_size == 5) {
    tmp <- df_sim |>
      filter(rank <= 5) |>
      select(id, franchise_id, rank)


    tmp_week_15 <- tmp |>
      mutate(join_id = case_when(
        rank == 5 ~ 4,
        rank == 4 ~ 5,
        TRUE ~ NA
      )) |>
      filter(!is.na(join_id)) |>
      left_join(tmp, by = c('join_id' = 'rank', 'id')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id.x, franchise_id.y, f_score)


    w15_winners <- tmp_week_15 |>
      left_join(tmp_week_15, by=c('id', 'franchise_id.x' = 'franchise_id.y')) |>
      mutate(
        w15_result = f_score.x > f_score.y
      ) |>
      filter(w15_result) |>
      select(id, franchise_id = franchise_id.x) |>
      mutate(result = TRUE)


    w16_teams <- tmp |>
      left_join(w15_winners, by = c('id', 'franchise_id')) |>
      filter(result | rank <= 3) |>
      group_by(id) |>
      mutate(
        join_id = case_when(
          rank == min(ifelse(rank <= 2, NA, rank), na.rm = TRUE) ~ 1,
          rank == max(ifelse(rank <= 2, NA, rank), na.rm = TRUE) ~ 2,
          rank == 1 ~ min(ifelse(rank <= 2, NA, rank), na.rm = TRUE),
          rank == 2 ~ max(rank),
          TRUE ~ NA
        )
      ) |>
      ungroup()


    tmp_week_16 <- w16_teams |>
      #filter(!is.na(join_id)) |>
      left_join(tmp |> mutate(rank2 = rank), by = c('id','join_id' = 'rank')) |>
      left_join(df_scores, by = c('franchise_id.x' = 'franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id = franchise_id.x, opponent_id = franchise_id.y, f_score)


    w16_winners <- tmp_week_16 |>
      left_join(tmp_week_16, by=c('id', 'opponent_id' = 'franchise_id')) |>
      mutate(
        w16_result = f_score.x > f_score.y
      ) |>
      filter(w16_result) |>
      select(id, franchise_id) |>
      mutate(result = TRUE)



    finals_teams <- tmp |>
      left_join(w16_winners, by = c('id', 'franchise_id')) |>
      filter(result) |>
      left_join(df_scores, by = c('franchise_id')) |>
      select(-data) |>
      mutate(
        f_score = map_dbl(.x = dens, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y)),
        #o_score = map(.x = dens.x, .f = ~sample(.x$x, 1, replace = TRUE, prob = .x$y))
      ) |>
      select(id, franchise_id, f_score)


    finals_winner <- finals_teams |>
      group_by(id) |>
      filter(f_score == max(f_score)) |>
      ungroup()

    round1 <- tmp |>
      left_join(w15_winners) |>
      filter(rank <= 3 | result) |>
      count(franchise_id) |>
      mutate(semi_finals = n/sims) |>
      select(-n)

    round2 <- w16_winners |>
      count(franchise_id) |>
      mutate(finals = n/sims) |>
      select(-n)

    winner <- finals_winner |>
      count(franchise_id) |>
      mutate(winner = n/sims) |>
      select(-n)

    return(
      round1 |>
        left_join(round2) |>
        left_join(winner)
    )

  }

  else {
    message('Error: playoff_size needs to be 4, 5, or 6')
  }



}


get_simulated_results <- function(df_season_sim, playoff_size = 6) {

  sims <- max(df_season_sim$id)
  franchises <- length(unique(df_season_sim$franchise_id))
  losers_bracket_ids <- seq(franchises - 3, franchises)

  df_season_sim |>
    group_by(id, franchise_id) |>
    summarise(
      w = sum(result),
      pf = sum(franchise_score_f),
      .groups = 'drop'
    ) |>
    group_by(id) |>
    arrange(-w, -pf) |>
    mutate(
      rank = row_number()
    ) |>
    ungroup() |>
    group_by(franchise_id) |>
    summarise(
      playoffs = sum(rank <= playoff_size)/sims,
      bye = sum(rank <= 3)/sims,
      loser_bracket = sum(rank <= 7)/sims, #sum(rank > franchises/2)/sims,
      last_place = sum(rank == franchises)/sims
    )

}
