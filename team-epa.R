library(cfbfastR)
library(tidyverse)
library(xgboost)
library(glue)
library(ggthemes)
library(ggplot2)

coach_data <- cfbd_coaches(
    min_year = 2014,
    max_year = 2020
)

pbp <- data.frame()
seasons <- 2014:2020
progressr::with_progress({
    future::plan("multisession")
    pbp <- cfbfastR::load_cfb_pbp(seasons)
})

cleaned <- pbp %>%
    filter(!is.na(EPA) & !is.na(home_wp_before)) %>%
    filter(season %in% seasons) %>%
    filter(home_wp_before >= .1 && home_wp_before <= .9) %>%
    select(game_play_number, season, pos_team, EPA)

base_data <- cleaned %>%
    left_join(coach_data, by = c("pos_team" = "school", "season" = "year")) %>%
    mutate(
        coach_name = glue("{first_name} {last_name}")
    )

graph_data <- base_data %>%
    group_by(coach_name) %>%
    filter(any(n() >= 300)) %>%
    arrange(season, game_play_number) %>%
    mutate(
        play_num = row_number(), #rows were in sequential order
        roll_epa = cumsum(EPA),
        car_epa = mean(EPA),
        diff = (roll_epa/play_num) - car_epa,
        abs_diff = abs(diff),
        lag_season = lag(season),
        lag_pos_team = lag(pos_team),
        change_pos_team = (lag_pos_team != pos_team),
        change_season = (lag_season != season)
    )%>%
    ungroup()

create_coach_chart <- function(name) {
    coach_filtered <- graph_data %>% filter(coach_name == name)
    p <- ggplot(coach_filtered, aes(x = play_num, y = diff)) +
        geom_point() +
        geom_smooth(method = "loess") +
        theme_fivethirtyeight() +
        theme(axis.title.x = element_text(), axis.title.y = element_text()) +
        labs(
            x = "Number of Snaps",
            y = "Average Residual from Career EPA/Play",
            title = glue("How Long Does It Take for a Coach's EPA/Play to Stabilize?"),
            subtitle = glue("Selected coach: {name} - Data from 2014 to 2020."),
            caption = glue("Created by Akshay Easwaran (@akeaswaran), based on QB chart from Conor McQuiston (@ConorMCQ5). Data from @cfbfastR.")
        )

    year_change = coach_filtered %>%
        filter(change_season == TRUE) %>%
        select(play_num, season)

    ann_text <- data.frame(
        x = c(),
        y = c(),
        lab = c()
    )

    for (i in 1:nrow(year_change)) {
        row <- year_change[i, ]

        print(glue("drawing line for play {row$play_num}"))
        p <- p + geom_vline(xintercept = row$play_num, linetype = "dashed")
        tmp <- data.frame(
            x = c(row$play_num + 40),
            y = c(max(coach_filtered$diff)),
            lab = c(as.character(row$season))
        )
        ann_text <- rbind(ann_text, tmp)
    }

    p <- p + geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 3, angle = 90)
    p
}
create_coach_chart(name = "Kirby Smart")
