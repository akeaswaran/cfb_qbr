library(cfbfastR)
library(tidyverse)
library(mgcv)

pbp <- data.frame()
seasons <- 2014:2020
progressr::with_progress({
future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})

cleaned <- pbp %>%
  filter(!is.na(passer_player_name) & !is.na(EPA) & !is.na(home_wp_before)) %>%
  filter(season %in% seasons) %>%
  select(season, play_text, week, rush, pass, passer_player_name, EPA, home_wp_before, yards_gained, play_type, fumble_vec) %>%
  mutate(
    home_wp = home_wp_before,
    qbr_epa = if_else(EPA < -5.0, -5.0, EPA),
    qbr_epa = if_else(fumble_vec == 1, -3.5, qbr_epa),
    weight = if_else(home_wp < .1 | home_wp > .9, .6, 1),
    weight = if_else((home_wp >= .1 & home_wp < .2) | (home_wp >= .8 & home_wp < .9), .9, weight),
    NULL
  )

qb_week <- cleaned %>%
  group_by(passer_player_name, season, week) %>%
  summarize(qbr_epa = weighted.mean(qbr_epa, weight))

qbr_scrape <- read.csv("./composite.csv") %>%
  rename(raw_qbr = TQBR)

j <- qb_week %>%
  left_join(qbr_scrape, by = c('passer_player_name' = 'athlete_name', 'week' = 'week', 'season' = 'year')) %>%
  filter(!is.na(raw_qbr))

cor(j$qbr_epa, j$raw_qbr, use = 'complete.obs')

qbr_model <- mgcv::gam(raw_qbr ~ s(qbr_epa),
                 data=j, method="REML")

save(qbr_model, file = './qbr_model.rda')
