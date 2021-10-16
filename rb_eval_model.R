library(tidyverse)
library(cfbfastR)
library(mgcv)
library(ggplot2)
library(glue)

seasons <- 2006:2019
pbp <- purrr::map_df(seasons, function(x) {
    # print(glue("loading data for year {x}"))
    readRDS(
        url(
            glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/pbp/rds/play_by_play_{x}.rds")
        )
    )
    # print(glue("downloaded data for year {x}"))
})

pbp_db <- pbp %>%
    select(game_id, text, yds_rushed, start_ydstogo, rusher_player_name, start_down, season, week, home_wp_before, posteam, rush, epa) %>%
    filter(rush == 1) %>%
    filter(!is.na(posteam) & !is.na(epa) & !is.na(rusher_player_name) & season >= 2006 & season <= 2019) %>%
    filter((rusher_player_name != "TEAM")) %>%
    mutate(
        fo_success = case_when(
            (start_down == 1) ~ (yds_rushed >= 0.5 * start_ydstogo),
            (start_down == 2) ~ (yds_rushed >= 0.7 * start_ydstogo),
            (start_down >= 3) ~ (yds_rushed >= start_ydstogo),
            TRUE ~ FALSE
        ),
        is_rush_opp = (yds_rushed >= 4),
        adj_yardage = ifelse(yds_rushed > 10, 10, yds_rushed),
        line_yards = case_when(
            (yds_rushed < 0) ~ (1.20 * adj_yardage),
            (yds_rushed %in% range(0, 4)) ~ adj_yardage,
            (yds_rushed >= 5) ~ (0.5 * adj_yardage),
            TRUE ~ 0
        ),
        second_level_yards = case_when(
            (yds_rushed >= 5) ~ (0.5 * (adj_yardage - 5)),
            TRUE ~ 0
        ),
        open_field_yards = case_when(
            (yds_rushed > 10) ~ (yds_rushed - adj_yardage),
            TRUE ~ 0
        ),
        highlight_yards = second_level_yards + open_field_yards
    ) %>%
    group_by(game_id) %>%
    mutate(
        season = first(season)
    ) %>%
    ungroup()

lrbs <- pbp_db %>%
    group_by(rusher_player_name, season) %>%
    mutate(
        unadjusted_epa = epa,
        epa = if_else(epa < -4.5, -4.5, epa)
    ) %>%
    summarize(
        n_opps = sum(is_rush_opp),
        n_plays = n(),
        unadjusted_epa = sum(unadjusted_epa) / n_plays,
        epa = sum(epa)/n_plays,
        success =sum(fo_success)/n_plays,
        highlight_yards = sum(highlight_yards)/n_opps,
        posteam = dplyr::last(posteam)
    ) %>%
    filter(n_plays > 100) %>%
    mutate(
        lepa = lag(epa, n = 1),
        lunad_epa = lag(unadjusted_epa, n = 1),
        lhlite_yds = lag(highlight_yards, n = 1),
        lsuccess = lag(success, n = 1),
        lplays = lag(n_plays),
        weight = (n_plays^2 + lplays^2)^.5
    ) %>% ungroup()

model_data <- lrbs %>%
    select(unadjusted_epa, lhlite_yds, lepa, lsuccess, weight, season) %>%
    dplyr::rename(
        target = unadjusted_epa,
        highlight_yards = lhlite_yds,
        epa_per_play = lepa,
        success = lsuccess
    )

rsq <- function (x, y) {
    return(round(cor(x, y) ^ 2, 4))
}

r2w <- function(y, y_pred, w) {
    # Calculate R2 using the correlation coefficient method
    xy = cbind(y, y_pred)
    return(boot::corr(d=xy, w=w) ^ 2)
}

cv_results <- map_dfr(seasons, function(x) {
    test_data <- model_data %>%
        filter(season == x)
    train_data <- model_data %>%
        filter(season != x)

    dakota_model = mgcv::gam(
        target ~ s(epa_per_play) + s(success), data = train_data, weights = weight
    )

    preds <- as.data.frame(
        matrix(predict(dakota_model, test_data))
    ) %>%
        dplyr::rename(exp_rb_epa = V1)

    cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
    return(cv_data)
})

#
# LOSO Calibration
show_calibration_chart <- function(bin_size) {
    calibration_results <- cv_results %>%
        filter(!is.na(highlight_yards) & !is.na(epa_per_play) & !is.na(target) & !is.na(exp_rb_epa)) %>%
        # Create BINS for wp:
        mutate(
            bin_pred_epa = round(exp_rb_epa / bin_size) * bin_size,
        ) %>%
        group_by(bin_pred_epa) %>%
        summarize(
            total_instances = n(),
            avg_epa = mean(target),
        ) %>%
        mutate(
            bin_actual_epa = avg_epa
        )

    y_max = max(calibration_results$bin_actual_epa)
    y_min = min(calibration_results$bin_actual_epa)
    x_max = max(calibration_results$bin_pred_epa)
    x_min = min(calibration_results$bin_pred_epa)

    cal_error <- calibration_results %>%
        ungroup() %>%
        mutate(cal_diff = abs(bin_pred_epa - bin_actual_epa)) %>%
        summarize(
            weight_cal_error = weighted.mean(cal_diff, total_instances, na.rm = TRUE)
        )

    ann_text <- data.frame(
        x = c((0.25 * (x_max - x_min) + x_min), 0.75 * (x_max - x_min) + x_min),
        y = c((0.75 * (y_max - y_min) + y_min), 0.25 * (y_max - y_min) + y_min),
        lab = c("Higher\nthan predicted", "Lower\nthan predicted")
    )

    r2 <- r2w(calibration_results$bin_actual_epa, calibration_results$bin_pred_epa, calibration_results$total_instances)

    cal_text <- data.frame(
        x = c(0.75 * (x_max - x_min) + x_min),
        y = c(0.125 * (y_max - y_min) + y_min),
        lab = c(glue("Wgt Cal Error: {round(cal_error$weight_cal_error, 4)}\nWgt R^2: {round(r2, 4)}"))
    )

    ggplot(calibration_results, aes(bin_pred_epa, bin_actual_epa)) +
        geom_point(aes(x = bin_pred_epa, y = bin_actual_epa, size = total_instances)) +
        # geom_smooth(aes(x = bin_pred_epa, y = bin_actual_epa), method = "loess") +
        geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
        coord_equal() +
        labs(
            size = "Number of rushers",
            x = "Expected Rushing EPA",
            y = "Actual Rushing EPA",
            title = glue("Calibrating xREPA with bin size {bin_size}")
        ) +
        geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 5) +
        geom_text(data = cal_text, aes(x = x, y = y, label = lab), size = 3) +
        xlim(x_min, x_max) +
        ylim(y_min, y_max) +
        theme_bw()
}
show_calibration_chart(bin_size = 0.05)
