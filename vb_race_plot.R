library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(glue)

generate_race_plot <- function(game_id, away_color, home_color, subtitle = "") {
    pbp <- ncaa_vb_pbp(game_id) %>%
        filter(!grepl("Sub", action_type), !grepl("Challenge", action_type), action_type != "Match End")
    away = pbp$away_team[1]
    home = pbp$home_team[1]

    palette <- setNames(object = c(away_color, home_color), nm = c(away, home))

    base <- ggplot(pbp) +
        geom_line(aes(x=match_action_number, y=as.integer(away_score), group=set, color=away)) +
        geom_line(aes(x=match_action_number, y=as.integer(home_score), group=set, color=home)) +
        theme_fivethirtyeight() +
        theme(axis.title.x = element_text(), axis.title.y = element_text()) +
        scale_color_manual(name="Team", values=palette) +
        labs(
            x = "Match Actions",
            y = "Score",
            title = glue("Race Plot: {away} vs {home}"),
            subtitle = subtitle,
            caption = glue("Created by Akshay Easwaran (@akeaswaran), data from ncaascrapR.")
        )

    set_change = pbp %>%
        filter(action_type == "Set End") %>%
        select(match_action_number, set, home_score, away_score) %>%
        mutate(
            lead_play_num = lead(match_action_number),
            lead_play_num = ifelse(is.na(match_action_number), max(coach_filtered$match_action_number), match_action_number),
            score = max(home_score, away_score)
        )

    ann_text <- data.frame(
        x = c(),
        y = c(),
        lab = c(),
        team_x = c(),
        team = c()
    )

    for (i in 1:nrow(set_change)) {
        row <- set_change[i, ]

        base <- base + geom_vline(xintercept = row$match_action_number, linetype = "dashed")
        x_team = (row$match_action_number + row$lead_play_num) / 2
        tmp <- data.frame(
            x = c(row$match_action_number + max(c(10, (0.1 * (x_team - row$match_action_number))))),
            y = c(max(as.integer(pbp$home_score)) * 0.95),
            lab = c(paste("End of Set", as.character(row$set), sep = " "))
        )
        ann_text <- rbind(ann_text, tmp)
    }

    base <- base + geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 3, angle = 90)
    ggsave(plot = base, filename = glue("race-plot-{away}-{home}.jpg"), width=11,height=7, dpi=320)
    return(base)
}
