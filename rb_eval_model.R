library(tidyverse)
library(cfbfastR)
library(mgcv)
library(ggplot2)
library(glue)

pbp <- data.frame()
seasons <- 2014:cfbfastR:::most_recent_season()
progressr::with_progress({
    future::plan("multisession")
    pbp <- cfbfastR::load_cfb_pbp(seasons)
})

pbp_db <- pbp %>%
    select(game_id, play_text, yards_gained, distance, rusher_player_name, down, season, week, home_wp_before, pos_team, def_pos_team, pass, rush, EPA, success) %>%
    filter(rush == 1) %>%
    filter(!is.na(pos_team) & !is.na(EPA) & !is.na(rusher_player_name)) %>%
    filter((rusher_player_name != "TEAM")) %>%
    mutate(
        fo_success = case_when(
            (down == 1) ~ (yards_gained >= 0.5 * distance),
            (down == 2) ~ (yards_gained >= 0.7 * distance),
            (down >= 3) ~ (yards_gained >= distance),
            TRUE ~ FALSE
        ),
        is_rush_opp = (yards_gained >= 4),
        adj_yardage = ifelse(yards_gained > 10, 10, yards_gained),
        line_yards = case_when(
            (yards_gained < 0) ~ (1.20 * adj_yardage),
            (yards_gained %in% range(0, 4)) ~ adj_yardage,
            (yards_gained >= 5) ~ (0.5 * adj_yardage),
            TRUE ~ 0
        ),
        second_level_yards = case_when(
            (yards_gained >= 5) ~ (0.5 * (adj_yardage - 5)),
            TRUE ~ 0
        ),
        open_field_yards = case_when(
            (yards_gained > 10) ~ (yards_gained - adj_yardage),
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
        unadjusted_epa = EPA,
        epa = if_else(EPA < -4.5, -4.5, EPA)
    ) %>%
    summarize(
        n_opps = sum(is_rush_opp),
        n_plays = n(),
        unadjusted_epa = sum(unadjusted_epa) / n_plays,
        epa = sum(EPA)/n_plays,
        success =sum(fo_success)/n_plays,
        highlight_yards = sum(highlight_yards)/n_opps,
        pos_team = dplyr::last(pos_team)
    ) %>%
    filter(n_opps > 25 & n_plays > 100) %>%
    mutate(
        lepa = lag(epa, n = 1),
        lunad_epa = lag(unadjusted_epa, n = 1),
        lhlite_yds = lag(highlight_yards, n = 1),
        lsuccess = lag(success, n = 1),
        lplays = lag(n_plays),
        weight = (n_plays^2 + lplays^2)^.5
    ) %>% ungroup()

model_data <- lrbs %>%
    select(unadjusted_epa, lhlite_yds, lepa, weight, season) %>%
    dplyr::rename(
        target = unadjusted_epa,
        highlight_yards = lhlite_yds,
        epa_per_play = lepa
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
        target ~ s(highlight_yards) + s(epa_per_play), data = train_data, weights = weight
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

    y_max = 0.25 # max(calibration_results$bin_actual_epa)
    y_min = -0.25 # min(calibration_results$bin_actual_epa)
    x_max = 0.25 # max(calibration_results$bin_pred_epa)
    x_min = -0.25 # min(calibration_results$bin_pred_epa)

    cal_error <- calibration_results %>%
        ungroup() %>%
        mutate(cal_diff = abs(bin_pred_epa - bin_actual_epa)) %>%
        summarize(
            weight_cal_error = weighted.mean(cal_diff, total_instances, na.rm = TRUE)
        )

    ann_text <- data.frame(
        x = c(-0.125, 0.125),
        y = c(0.125, -0.125),
        lab = c("Higher\nthan predicted", "Lower\nthan predicted")
    )

    r2 <- r2w(calibration_results$bin_actual_epa, calibration_results$bin_pred_epa, calibration_results$total_instances)

    cal_text <- data.frame(
        x = c(0.1875),
        y = c(-0.225),
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
show_calibration_chart(bin_size = 0.025)
