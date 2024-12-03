library(tidyverse)
library(ffscrapr)
library(duckdb)


source("helper_plot.R")

my_league <- ff_connect("sleeper", "1121217308480954368", season = 2024)
my_league

WEEK <- 12
SIMS <- 10000
PLAYOFF_SIZE <- 4

df <- ffscrapr::ff_schedule(my_league)
franchises <- ffscrapr::ff_franchises(my_league)
tmp <- df |> filter(week <= WEEK)
tmp <- setDT(tmp)
df_schedule <- setDT(df)
library(data.table)
ind_samples <- tmp[
  , .(ind_sample = list(franchise_score))
  , by = franchise_id
]
group_samples <- tmp[
  , .(group_sample = list(franchise_score))
]


empirical_bayes_kde_with_tail <- function(ind_data, pop_kde, weight = 0.5, tail_weight = 0.05) {
  # Calculate individual KDE
  ind_kde <- density(ind_data, adjust = 1)

  # Extend the individual KDE range with a light tail based on population KDE
  extended_x <- c(
    seq(min(pop_kde$x), min(ind_kde$x), length.out = 50),
    ind_kde$x,
    seq(max(ind_kde$x), max(pop_kde$x), length.out = 50)
  )
  extended_y <- c(
    rep(tail_weight, 50),
    ind_kde$y,
    rep(tail_weight, 50)
  )

  # Normalize the extended individual KDE
  extended_y <- extended_y / sum(extended_y)

  # Blend with population KDE
  pop_y_interp <- approx(pop_kde$x, pop_kde$y, xout = extended_x)$y
  pop_y_interp[is.na(pop_y_interp)] <- 0 # Handle NAs

  blended_y <- (1 - weight) * extended_y + weight * pop_y_interp

  # Normalize blended KDE
  blended_y <- blended_y / sum(blended_y)

  # Construct the blended KDE
  blended_kde <- list(x = extended_x, y = blended_y)
  class(blended_kde) <- "density"

  return(blended_kde)
}

scores_nested <- cbind(ind_samples, group_samples)
scores_nested[, `:=`(
    ind_kde = lapply(ind_sample, density),
    group_kde = lapply(group_sample, density))
]
scores_nested[, `:=` (
    simulated_kde = purrr::map2(.x = ind_sample, .y = group_kde, .f = ~ empirical_bayes_kde_with_tail(.x, .y, weight = 0.8, tail = 0.05))
  )
]
scores_nested <- scores_nested[, .(franchise_id, scores = purrr::map(.x = simulated_kde, .f = ~ sample(.x$x, 10000, replace = TRUE, prob = .x$y)))]
scores_nested

simulated_scores <- merge(df_schedule, scores_nested, by = "franchise_id", all.x = TRUE)[
  , .(opponent_id, franchise_score, opponent_score, scores = unlist(scores)), # Unnest scores
    by = .(franchise_id, week),
][
  , sim_id := seq_len(.N), # Add row numbers within groups
  by = .(franchise_id, week)
]

# Perform the self-join
simulated_season <- merge(
  simulated_scores,
  simulated_scores[, .(week, opponent_id = franchise_id, sim_id, scores_opp = scores)],
  by = c("opponent_id", "sim_id", "week"),
  all.x = TRUE
)[, `:=` (
    win = ifelse(week <= WEEK, scores > scores_opp, scores < scores_opp),
    pf = ifelse(week <= WEEK, franchise_score, scores),
    pa = ifelse(week <= WEEK, opponent_score, scores_opp)
  )
]

SIMS <- 10000
franchises <- setDT(franchises)

standings <- simulated_season[franchises[, .(franchise_id, franchise_name)]
  , on = .(franchise_id),
  nomatch = NULL
][
  , .(wins = sum(win)/SIMS, pf = sum(pf) / SIMS),
  by = .(franchise_id, franchise_name)
][
  order(-wins, franchise_name)
]


simulated_standings <- simulated_season[
  , .(wins = sum(win), pf = sum(pf)),
  by = .(sim_id, franchise_id)
][
  , `:=`(rn = seq_len(.N)), by = .(week)
]

simulated_standings <- setorder(simulated_season[, .(wins = sum(win), pf = sum(pf)), keyby = .(
  sim_id,
  franchise_id
)], -wins, -pf, na.last = TRUE)[, `:=`(standing = seq_len(.N)),
  by = .(sim_id)
]
