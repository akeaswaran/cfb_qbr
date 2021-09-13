library(cfbfastR)
library(tidyverse)
library(mgcv)
library(glue)

pbp <- data.frame()
seasons <- 2014:2020
progressr::with_progress({
future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})

cleaned <- pbp %>%
  filter(!is.na(EPA) & !is.na(home_wp_before)) %>%
  filter(season %in% seasons) %>%
  select(season, play_text, week, rush, pass, passer_player_name, rusher_player_name, EPA, home_wp_before, yards_gained, play_type, fumble_vec, penalty_flag, pass_attempt, sack_vec) %>%
  mutate(
    home_wp = home_wp_before,
    qbr_epa = if_else(EPA < -5.0, -5.0, EPA),
    qbr_epa = if_else(fumble_vec == 1, -3.5, qbr_epa),
    weight = if_else(home_wp < .1 | home_wp > .9, .6, 1),
    weight = if_else((home_wp >= .1 & home_wp < .2) | (home_wp >= .8 & home_wp < .9), .9, weight),
    athlete_name = case_when(
      !is.na(passer_player_name) ~ passer_player_name,
      !is.na(rusher_player_name) ~ rusher_player_name,
      TRUE ~ "NA"
    )
  )

qb_week <- cleaned %>%
  group_by(athlete_name, season, week) %>%
  summarize(qbr_epa = weighted.mean(qbr_epa, weight))

qbr_scrape <- read.csv("./composite_qbr.csv") %>%
  filter(season_type == 2) %>%
  rename(raw_qbr = QBR)

j <- qb_week %>%
  left_join(qbr_scrape, by = c('athlete_name' = 'athlete_name', 'week' = 'week', 'season' = 'season')) %>%
  filter(!is.na(raw_qbr))

clean_model_data <- j %>%
  mutate(label = raw_qbr) %>%
  ungroup() %>%
  select(season, qbr_epa, label)

cv_results <- map_dfr(seasons, function(x) {
  test_data <- clean_model_data %>%
    filter(season == x) %>%
    select(-season)
  train_data <- clean_model_data %>%
    filter(season != x) %>%
    select(-season)

  qbr_model <- mgcv::gam(label ~ s(qbr_epa), data=train_data, method="REML")

  preds <- as.data.frame(
    matrix(predict(qbr_model, test_data))
  ) %>%
    dplyr::rename(exp_qbr = V1)

  cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
  return(cv_data)
})

rsq <- function (x, y) {
  return(round(cor(x, y) ^ 2, 4))
}

r2w <- function(y, y_pred, w) {
  # Calculate R2 using the correlation coefficient method
  xy = cbind(y, y_pred)
  return(boot::corr(d=xy, w=w) ^ 2)
}

show_calibration_chart <- function(bin_size) {
  calibration_results <- cv_results %>%
    # Create BINS for wp:
    mutate(
      bin_pred_qbr = round(exp_qbr / bin_size) * bin_size,
    ) %>%
    group_by(bin_pred_qbr) %>%
    summarize(
      total_instances = n(),
      bin_actual_qbr = mean(label),
    )

  cal_error <- calibration_results %>%
    ungroup() %>%
    mutate(cal_diff = abs(bin_pred_qbr - bin_actual_qbr)) %>%
    summarize(
      weight_cal_error = weighted.mean(cal_diff, total_instances, na.rm = TRUE)
    )

  ann_text <- data.frame(
    x = c(25, 75),
    y = c(75, 25),
    lab = c("Higher\nthan predicted", "Lower\nthan predicted")
  )

  r2 <- r2w(calibration_results$bin_actual_qbr, calibration_results$bin_pred_qbr, calibration_results$total_instances)

  cal_text <- data.frame(
    x = c(87.5),
    y = c(0),
    lab = c(glue("Wgt Cal Error: {round(cal_error$weight_cal_error, 4)}\nWgt R^2: {round(r2, 4)}"))
  )

  ggplot(calibration_results, aes(bin_pred_qbr, bin_actual_qbr)) +
    geom_point(aes(x = bin_pred_qbr, y = bin_actual_qbr, size = total_instances)) +
    geom_smooth(aes(x = bin_pred_qbr, y = bin_actual_qbr), method = "loess") +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
    coord_equal() +
    labs(
      size = "Number of passers",
      x = "Expected QBR",
      y = "Actual QBR",
      title = glue("Calibrating xQBR with bin size {bin_size}")
    ) +
    geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 5) +
    geom_text(data = cal_text, aes(x = x, y = y, label = lab), size = 3) +
    xlim(0, 100) +
    ylim(0, 100) +
    theme_bw()
}
show_calibration_chart(bin_size = 2.5)
