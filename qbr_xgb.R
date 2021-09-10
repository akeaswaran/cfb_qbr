library(cfbfastR)
library(tidyverse)
library(xgboost)

pbp <- data.frame()
seasons <- 2014:2021
progressr::with_progress({
    future::plan("multisession")
    pbp <- cfbfastR::load_cfb_pbp(seasons)
})

cleaned <- pbp %>%
    filter(!is.na(EPA) & !is.na(home_wp_before)) %>%
    filter(season %in% seasons) %>%
    select(season, play_text, week, rush, pass, passer_player_name, rusher_player_name, EPA, home_wp_before, yards_gained, play_type, fumble_vec, penalty_flag) %>%
    mutate(
        home_wp = home_wp_before,
        qbr_epa = if_else(EPA < -5.0, -5.0, EPA),
        qbr_epa = if_else(fumble_vec == 1, -3.5, qbr_epa),
        weight = if_else(home_wp < .1 | home_wp > .9, .6, 1),
        weight = if_else((home_wp >= .1 & home_wp < .2) | (home_wp >= .8 & home_wp < .9), .9, weight),
        non_fumble_sack = ((grepl(" sack", play_text) == TRUE) & (fumble_vec == 0)),
        sack_epa = if_else(non_fumble_sack, qbr_epa, NaN),
        sack_weight = if_else(non_fumble_sack, weight, NaN),
        pass_epa = if_else((pass == 1), qbr_epa, NaN),
        pass_weight = if_else((pass == 1), weight, NaN),
        rush_epa = if_else((rush == 1), qbr_epa, NaN),
        rush_weight = if_else((rush == 1), weight, NaN),
        pen_epa = if_else((penalty_flag == 1), qbr_epa, NaN),
        pen_weight = if_else((penalty_flag == 1), weight, NaN),
        action_play = (EPA != 0),
        athlete_name = case_when(
            !is.na(passer_player_name) ~ passer_player_name,
            !is.na(rusher_player_name) ~ rusher_player_name,
            TRUE ~ "NA"
        )
    )

qb_week <- cleaned %>%
    filter(athlete_name != "NA") %>%
    group_by(athlete_name, season, week) %>%
    summarize(
        total_plays = n(),
        action_plays = sum(action_play),
        qbr_epa = weighted.mean(qbr_epa, weight),
        sack_epa = weighted.mean(sack_epa, sack_weight),
        pass_epa = weighted.mean(pass_epa, pass_weight),
        rush_epa = weighted.mean(rush_epa, rush_weight),
        pen_epa = weighted.mean(pen_epa, pen_weight)
    ) %>%
    mutate(
        qbr_epa = replace_na(qbr_epa, 0),
        sack_epa = replace_na(sack_epa, 0),
        pass_epa = replace_na(pass_epa, 0),
        rush_epa = replace_na(rush_epa, 0),
        pen_epa = replace_na(pen_epa, 0)
    )

qbr_scrape <- read.csv("./composite_qbr.csv") %>%
    filter(season_type == 2) %>%
    rename(raw_qbr = QBR)

model_data <- qb_week %>%
    left_join(qbr_scrape, by = c('athlete_name' = 'athlete_name', 'week' = 'week', 'season' = 'season')) %>%
    filter(!is.na(raw_qbr))

# ------ START XGB METHOD ------
#
# Params from nflfastR WP
nrounds <- 100
params <-
    list(
        booster = "gbtree",
        objective = "reg:squarederror",
        eval_metric = c("mae"),
        eta = 0.2,
        gamma = 0,
        subsample = 0.8,
        colsample_bytree = 0.8,
        max_depth = 4,
        min_child_weight = 1
    )

clean_model_data <- model_data %>%
    mutate(label = raw_qbr) %>%
    ungroup()

test_data <- model_data %>%
    filter(season == 2021) %>%
    mutate(label = raw_qbr) %>%
    ungroup() %>%
    select(qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, label)
train_data <- model_data %>%
    filter(season != 2020) %>%
    mutate(label = raw_qbr) %>%
    ungroup() %>%
    select(qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, label)

full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-label)),
                                   label = train_data$label
)
xgb_qbr_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

test_model_data <- clean_model_data %>% select(qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, label)

full_test <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = test_model_data %>% select(-label)),
                                  label = test_model_data$label
)
preds <- predict(xgb_qbr_model, full_test)
clean_model_data$exp_qbr <- preds

# ------ END XGB METHOD ------
#

calibration_results <- clean_model_data %>%
    # Create BINS for wp:
    mutate(
        bin_pred_qbr = round(exp_qbr / 2.5) * 2.5,
    ) %>%
    group_by(bin_pred_qbr) %>%
    summarize(
        total_instances = n(),
        avg_qbr = mean(raw_qbr),
        bin_actual_qbr = round(avg_qbr / 2.5) * 2.5
    )
mean(abs(calibration_results$bin_pred_qbr - calibration_results$bin_actual_qbr))

ann_text <- data.frame(
    x = c(25, 75), y = c(75, 25),
    lab = c("Higher\nthan expected", "Lower\nthan expected")
)

ggplot(calibration_results, aes(bin_pred_qbr, bin_actual_qbr)) +
    geom_point(aes(x = bin_pred_qbr, y = bin_actual_qbr, size = total_instances)) +
    geom_smooth(aes(x = bin_pred_qbr, y = bin_actual_qbr), method = "loess") +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
    coord_equal() +
    labs(
        size = "Number of passers",
        x = "Estimated QBR",
        y = "Observed QBR"
    ) +
    geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 5) +
    theme_bw()

clean_model_data %>%
    select(season, week, athlete_name, team_abbreviation, opponent, qbr_epa, TQBR, raw_qbr, exp_qbr) %>%
    arrange(season, week) %>%
    rename(team = team_abbreviation) %>%
    write.csv("xqbr_xgb.csv", row.names=FALSE)
