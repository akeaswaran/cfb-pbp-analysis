library(cfbfastR)
library(tidyverse)
library(xgboost)
library(glue)
library(ggthemes)
library(ggplot2)
library(cfbplotR)
library(Rcpp)

seasons <- 2014:2020
coach_data <- cfbd_coaches(
    min_year = min(seasons),
    max_year = max(seasons)
)

pbp <- data.frame()
progressr::with_progress({
    future::plan("multisession")
    pbp <- cfbfastR::load_cfb_pbp(seasons)
})

cleaned <- pbp %>%
    filter(!is.na(EPA) & !is.na(home_wp_before)) %>%
    filter(season %in% seasons) %>%
    filter(home_wp_before >= .1 && home_wp_before <= .9) %>%
    select(season, week, game_play_number, pos_team, EPA)

base_data <- cleaned %>%
    left_join(coach_data, by = c("pos_team" = "school", "season" = "year")) %>%
    mutate(
        coach_name = glue("{first_name} {last_name}")
    )

graph_data <- base_data %>%
    group_by(coach_name) %>%
    filter(any(n() >= 300)) %>%
    arrange(season, week, game_play_number) %>%
    mutate(
        play_num = row_number(), #rows were in sequential order
        roll_epa = cumsum(EPA),
        car_epa = mean(EPA),
        diff = (roll_epa/play_num) - car_epa,
        abs_diff = abs(diff),
        lag_season = lag(season),
        lag_season = ifelse(is.na(lag_season), TRUE, lag_season),
        change_season = (lag_season != season)
    )%>%
    ungroup()

create_coach_chart <- function(name, show_reg = FALSE, save_img = FALSE) {
    coach_filtered <- graph_data %>%
        filter(
           (coach_name == name)
           & (season %in% seasons)
        )
    p <- ggplot(coach_filtered, aes(x = play_num, y = diff)) +
        geom_point() +
        theme_fivethirtyeight() +
        theme(axis.title.x = element_text(), axis.title.y = element_text()) +
        labs(
            x = "Number of Snaps",
            y = "Average Residual from Career EPA/Play",
            title = glue("How Long Does It Take for a Coach's EPA/Play to Stabilize?"),
            subtitle = glue("Selected coach: {name} - Data from {min(seasons)} to {max(seasons)} with win probability btwn 10% and 90%."),
            caption = glue("Created by Akshay Easwaran (@akeaswaran), based on QB chart from Conor McQuiston (@ConorMCQ5).\nData from @cfbfastR, logos from cfbplotR.")
        )

    if (show_reg) {
        p <- p + geom_smooth(method = "loess")
    }

    year_change = coach_filtered %>%
        filter(change_season == TRUE) %>%
        select(play_num, season, pos_team) %>%
        mutate(
            lead_play_num = lead(play_num),
            lead_play_num = ifelse(is.na(lead_play_num), max(coach_filtered$play_num), lead_play_num)
        )

    ann_text <- data.frame(
        x = c(),
        y = c(),
        lab = c(),
        team_x = c(),
        team = c()
    )

    for (i in 1:nrow(year_change)) {
        row <- year_change[i, ]

        print(glue("drawing line for play {row$play_num} with lead {row$lead_play_num}"))
        p <- p + geom_vline(xintercept = row$play_num, linetype = "dashed")
        x_team = (row$play_num + row$lead_play_num) / 2
        tmp <- data.frame(
            x = c(row$play_num + min(c(50, (0.1 * (x_team - row$play_num))))),
            y = c(max(coach_filtered$diff) * 0.97),
            lab = c(as.character(row$season)),
            team_x = c(x_team),
            team = c(row$pos_team)
        )
        ann_text <- rbind(ann_text, tmp)
    }

    p <- p + geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 3, angle = 90)
    p <- p + geom_cfb_logos(data = ann_text, aes(x = team_x, y = y, team = team, alpha = 1.0), width = 0.0625)
    if (save_img) {
        ggsave(plot = p, filename = glue("epa-prog-{name}.jpg"), width=10.4,height=6.25, dpi=320)
    }
    p
}
create_coach_chart(name = "Paul Johnson", save_img = TRUE)

create_team_chart <- function(name, year, show_reg = FALSE, save_img = FALSE) {
    team_filtered <- base_data %>%
        filter(
            (pos_team == name)
            & (season %in% seasons)
            & (season == year)
        ) %>%
        arrange(season, week, game_play_number) %>%
        mutate(
            play_num = row_number(), #rows were in sequential order
            roll_epa = cumsum(EPA),
            car_epa = mean(EPA),
            diff = (roll_epa/play_num) - car_epa,
            abs_diff = abs(diff),
            lag_season = lag(season),
            lag_season = ifelse(is.na(lag_season), TRUE, lag_season),
            change_season = (lag_season != season)
        )%>%
        ungroup()

    p <- ggplot(team_filtered, aes(x = play_num, y = diff)) +
        geom_point() +
        theme_fivethirtyeight() +
        theme(axis.title.x = element_text(), axis.title.y = element_text()) +
        labs(
            x = "Number of Snaps",
            y = "Average Residual from Career EPA/Play",
            title = glue("How Long Does It Take for a Team's EPA/Play to Stabilize?"),
            subtitle = glue("Selected team: {name} - Data from {year} with win probability btwn 10% and 90%."),
            caption = glue("Created by Akshay Easwaran (@akeaswaran), based on QB chart from Conor McQuiston (@ConorMCQ5).\nData from @cfbfastR, logos from cfbplotR.")
        )

    if (show_reg) {
        p <- p + geom_smooth(method = "loess")
    }

    if (save_img) {
        ggsave(plot = p, filename = glue("epa-prog-{name}-{year}.jpg"), width=10.4,height=6.25, dpi=320)
    }
    p
}

create_team_chart(name = "Georgia Tech", year = 2014, save_img = TRUE)
