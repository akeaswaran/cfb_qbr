library(ggplot2)
library(ggthemes)
library(tidyverse)
library(glue)

qbr_csv <- read.csv("./composite_qbr.csv") %>%
    mutate(
        QBR = as.numeric(QBR),
        TQBR = as.numeric(TQBR),
    )

base_gt_qbr <- qbr_csv[which(qbr_csv$team_abbreviation == "GT"), ]

gt_qbr <- base_gt_qbr %>%
    group_by(season, athlete_name) %>%
    summarize(
        total_games = n(),
        avg_qbr = mean(QBR),
        avg_adj_qbr = mean(TQBR)
    ) %>%
    ungroup()

ggplot(gt_qbr, aes(season, avg_adj_qbr)) +
    geom_point(aes(x = season, y = avg_adj_qbr, size=total_games, group=athlete_name, color=athlete_name)) +
    scale_x_continuous(breaks = round(seq(min(gt_qbr$season), max(gt_qbr$season), by = 1.0),1)) +
    geom_line(aes(x=season, y=avg_adj_qbr, color = athlete_name, group = athlete_name)) +
    labs(
        size = "Number of appearances",
        group = "Quarterback",
        color = "Quarterback",
        x = "Season",
        y = "Average ESPN QBR",
        title = "Georgia Tech QBR history",
        subtitle = glue("Seasons: {min(gt_qbr$season)} to {max(gt_qbr$season)}; adjusted for opponent"),
        caption = "Created by Akshay Easwaran (@akeaswaran). Data from ESPN, retrieved via ESPN API."
    ) +
    theme_fivethirtyeight()
