library(cfbfastR)
library(tidyverse)
library(xgboost)
library(glue)

pbp <- data.frame()
seasons <- 2014:2021
progressr::with_progress({
    future::plan("multisession")
    pbp <- cfbfastR::load_cfb_pbp(seasons)
})

spreads <- data.frame()
for (yr in seasons) {
    for (wk in 1:16) {
        tmp <- cfbd_betting_lines(year = yr, week = wk)
        spreads <- rbind(spreads, tmp)
    }
}

spreads <- spreads %>%
    group_by(game_id) %>%
    mutate(
        home_team_spread = -1 * as.double(spread)
    ) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    select(game_id, home_team_spread)

cleaned <- pbp %>%
    filter(!is.na(EPA) & !is.na(home_wp_before)) %>%
    filter(season %in% seasons) %>%
    select(season, game_id, play_text, week, rush, pass, passer_player_name, rusher_player_name, EPA, home_wp_before, yards_gained, play_type, fumble_vec, penalty_flag, pass_attempt, sack_vec, home, away, pos_team) %>%
    mutate(
        home_wp = home_wp_before,
        qbr_epa = if_else(EPA < -5.0, -5.0, EPA),
        qbr_epa = if_else(fumble_vec == 1, -3.5, qbr_epa),
        weight = if_else(home_wp < .1 | home_wp > .9, .6, 1),
        weight = if_else((home_wp >= .1 & home_wp < .2) | (home_wp >= .8 & home_wp < .9), .9, weight),
        non_fumble_sack = ((sack_vec == 1) & (fumble_vec == 0)),
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

base_qb_week <- cleaned %>%
    left_join(spreads, by = c('game_id' = 'game_id')) %>%
    filter(!is.na(home_team_spread)) %>%
    mutate(
        pos_team_spread = ifelse(home == pos_team, home_team_spread, -1 * home_team_spread),
        opponent = ifelse(home == pos_team, away, home)
    )

qb_week <- base_qb_week %>%
    filter(athlete_name != "NA") %>%
    group_by(athlete_name, season, week) %>%
    summarize(
        total_plays = n(),
        action_plays = sum(action_play),
        qbr_epa = weighted.mean(qbr_epa, weight, na.rm = TRUE),
        sack_epa = weighted.mean(sack_epa, sack_weight, na.rm = TRUE),
        pass_epa = weighted.mean(pass_epa, pass_weight, na.rm = TRUE),
        rush_epa = weighted.mean(rush_epa, rush_weight, na.rm = TRUE),
        pen_epa = weighted.mean(pen_epa, pen_weight, na.rm = TRUE),
        spread = first(pos_team_spread),
        game_id = first(game_id),
        opponent = first(opponent)
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
    rename(raw_qbr = QBR, adj_qbr = TQBR)

model_data <- qb_week %>%
    left_join(qbr_scrape, by = c('game_id' = 'game_id', 'athlete_name' = 'athlete_name', 'week' = 'week', 'season' = 'season')) %>%
    filter(!is.na(raw_qbr) & !is.na(adj_qbr))
# ------ START XGB METHOD ------
#
# Params from nflfastR WP
nrounds <- 25
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
    mutate(label = adj_qbr) %>%
    ungroup() %>%
    select(season, qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, spread, adj_qbr, label)

cv_results <- map_dfr(seasons, function(x) {
    test_data <- clean_model_data %>%
        filter(season == x) %>%
        select(-season)
    train_data <- clean_model_data %>%
        filter(season != x) %>%
        select(-season)

    full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-label, -adj_qbr)),
                                       label = train_data$label
    )
    xgb_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

    preds <- as.data.frame(
        matrix(predict(xgb_model, as.matrix(test_data %>% select(-label, -adj_qbr))))
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

# ------ END XGB METHOD ------
#
# LOSO Calibration
show_calibration_chart <- function(bin_size) {
    calibration_results <- cv_results %>%
        # Create BINS for wp:
        mutate(
            bin_pred_qbr = round(exp_qbr / bin_size) * bin_size,
        ) %>%
        group_by(bin_pred_qbr) %>%
        summarize(
            total_instances = n(),
            # min_raw_qbr = min(raw_qbr),
            # max_raw_qbr = max(raw_qbr),
            avg_qbr = mean(adj_qbr),
            # bin_actual_qbr = round(avg_qbr / bin_size) * bin_size,
            # min_exp_qbr = min(exp_qbr),
            # max_exp_qbr = max(exp_qbr),
        ) %>%
        mutate(
            bin_actual_qbr = avg_qbr # cheating to not change a ton of code
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
show_calibration_chart(bin_size = 5.0)

# compose final model to save
model_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = clean_model_data %>% select(-season, -label, -adj_qbr)),
    label = clean_model_data$label
)
xgb_qbr_model <- xgboost::xgboost(params = params, data = model_train, nrounds = nrounds, verbose = 2)
xgb.save(xgb_qbr_model, "./qbr_model.model")

# add vars to model data and push csv
model_data <- model_data %>%
    ungroup()
model_data$exp_qbr <- matrix(predict(xgb_qbr_model, as.matrix(model_data %>% select(qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, spread))))

model_data %>%
    select(season, week, athlete_name, team_abbreviation, opponent, qbr_epa, pass_epa, rush_epa, sack_epa, pen_epa, spread, adj_qbr, exp_qbr) %>%
    rename(team = team_abbreviation) %>%
    write.csv("xqbr_xgb.csv")
