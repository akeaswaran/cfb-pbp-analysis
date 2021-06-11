library(cfbfastR)
library(tidyverse)
library(ggplot2)
library(ggthemes)

years <- 2014:2020

pbp_data <- data.frame()
progressr::with_progress({
    future::plan("multisession")
    pbp_data <- cfbfastR::load_cfb_pbp(years)
})

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

pbp_data <- pbp_data %>%
    filter(
        !is.na(offense_conference)
        & !is.na(defense_conference)
    )

kicker_types <- c("Field Goal Good", "Field Goal Missed")

cimaglia_data <- pbp_data %>%
    filter(
        (play_type %in% kicker_types)
    ) %>%
    mutate(
        kicker = case_when(
            ((grepl("Brent Cimaglia", play_text) == TRUE) | (grepl("B.Cimaglia", play_text) == TRUE) | (grepl("B. Cimaglia", play_text) == TRUE)) ~ "Brent Cimaglia",
            TRUE ~ pos_team
        )
    )

generic_fg <- cimaglia_data %>%
    filter(
        (play_type != "Kickoff")
        & (!is.na(EPA))
    ) %>%
    mutate(
        fg_distance = yards_to_goal + 17,
        yd_bin = round_any(fg_distance, 10, f = floor),
        make = grepl("Good", play_type),
    ) %>%
    filter(
        fg_distance < 60
    ) %>%
    group_by(yd_bin,kicker) %>%
    summarize(total_kicks = n(), accuracy = mean(make), total_EPA = sum(EPA), avg_EPA = mean(EPA)) %>%
    mutate(
        cat_title = paste0(yd_bin,"-",(yd_bin + 9)),
    )

cimaglia_fg <- generic_fg %>%
    filter(
        ((kicker == "Brent Cimaglia") | (kicker == "Georgia Tech"))
    ) %>%
    mutate(
        vjust=ifelse(avg_EPA >= 0, 1.6, 1.6),
        title_col=ifelse(avg_EPA >= 0, "white", "black")
    )

ggplot(data=cimaglia_fg, aes(x=cat_title, y=avg_EPA,fill=kicker)) +
    geom_bar(position="dodge",stat="identity") +
    geom_text(position = position_dodge2(width = 0.9, preserve = "single"), aes(vjust=vjust, label=total_kicks),color=cimaglia_fg$title_col) +
    labs(title="Brent Cimaglia: Kicking Revolution",
         subtitle="Data from 2017 to 2020 seasons. Numbers atop/under bars indicate sample size.",
         caption="Data from @cfbfastR. Chart by From the Rumble Seat (@FTRSBlog)."
    ) +
    xlab("FG Distance (yd)") +
    ylab("EPA/Attempt") +
    theme_fivethirtyeight() +
    scale_fill_manual(values=c("#FF8200","#B3A369")) +
    theme(axis.title = element_text())

ggplot(graphic_fg %>% filter(kicker != "Georgia Tech" && kicker != "Brent Cimaglia")) +
    geom_point(aes(x = cat_title, y = avg_EPA), size = 3, alpha = 0.025) +
    geom_point(data = graphic_fg %>% filter(kicker == "Georgia Tech"), aes(x = cat_title, y = avg_EPA, color = "Georgia Tech"), size = 3) +
    geom_point(data = graphic_fg %>% filter(kicker == "Brent Cimaglia"), aes(x = cat_title, y = avg_EPA, color="Brent Cimaglia"), size = 3) +
    theme_fivethirtyeight() +
    labs(
        x ="FG Distance (yds)",
        y = "EPA/Attempt",
        color = "Kicker"
    ) +
    aes(group=rev(avg_EPA)) +
    labs(title="Brent Cimaglia: Kicking Revolution",
         subtitle="Data from 2017 to 2020 seasons. Faded dots represent rest of nation.",
         caption="Data from @cfbfastR. Chart by From the Rumble Seat (@FTRSBlog)."
    ) +
    scale_color_manual(values = c("Georgia Tech" = "#b3a369", "Brent Cimaglia" = "#FF8200")) +
    theme(axis.title = element_text())

georgia_tech_false_starts <- pbp_data %>%
    filter(
        (grepl("false start", penalty_play_text)
        | grepl("false start", penalty_detail)
        | grepl("False start", penalty_play_text)
        | grepl("False start", penalty_detail)
        | grepl("False Start", penalty_play_text)
        | grepl("False Start", penalty_detail))
        & (pos_team == "Georgia Tech")
        & (grepl("Jordan Williams", play_text)
           | grepl("J.Williams", play_text)
           | grepl("J. Williams", play_text))
    )
