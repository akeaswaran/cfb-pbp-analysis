library(tidyverse)
library(dplyr)
library(ggplot2)

seasons = data.frame(start = 2017:2022) %>%
    dplyr::mutate(end = start + 1)

events_list = purrr::map2(seasons$start, seasons$end, function(start, end) {
    readr::read_csv(glue::glue("https://raw.githubusercontent.com/statsbomb/amf-open-data/main/data/events/tb12_events_dataset_{start}_{end}.csv"))
})

plays_list = purrr::map2(seasons$start, seasons$end, function(start, end) {
    readr::read_csv(glue::glue("https://raw.githubusercontent.com/statsbomb/amf-open-data/main/data/plays/tb12_plays_dataset_{start}_{end}.csv"))
})

events = dplyr::bind_rows(events_list)
plays = dplyr::bind_rows(plays_list)

passes <- events %>%
    dplyr::filter(
        grepl("Pass",event_types)
        & !grepl("Fake Pass",event_types)
    ) %>%
    dplyr::left_join(plays, by = c("play_uuid", "game_id")) %>%
    dplyr::filter(
        !is.na(play_pass_made)
        & !is.na(event_success)
    ) %>%
    dplyr::select(-`...1.x`, -`...1.y`)

pass_data = passes %>%
    dplyr::select(
        label = event_success,

        season = season.y,
        event_pass_air_yards,
        play_target_separation,
        event_pass_target_x,
        event_pass_target_y,
        play_qb_pressure,
    ) %>%
    dplyr::mutate(
        endline_receiver_dist = 110 - event_pass_target_x,
        sideline_receiver_dist = dplyr::case_when(
            (53.33 - event_pass_target_y) < event_pass_target_y ~ (53.33 - event_pass_target_y),
            .default = event_pass_target_y
        ),
        play_qb_pressure = dplyr::case_when(
            is.na(play_qb_pressure) ~ F,
            .default = play_qb_pressure
        )
    ) %>%
    dplyr::select(
        -event_pass_target_x,
        -event_pass_target_y
    ) %>%
    dplyr::filter(
        !is.na(event_pass_air_yards)
        & !is.na(play_target_separation)
        & !is.na(endline_receiver_dist)
        & !is.na(sideline_receiver_dist)
        & !is.na(play_qb_pressure)
    )

seasons = unique(pass_data$season)
nrounds <- 560
params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.025,
    gamma = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 6,
    base_score = mean(pass_data$label)
)
cv_results_raw <- purrr::map_dfr(seasons, function(x) {
    test_data <- pass_data %>%
        filter(season == x) %>%
        select(-season)
    train_data <- pass_data %>%
        filter(season != x) %>%
        select(-season)

    full_train <- xgboost::xgb.DMatrix(
        model.matrix(~ . + 0, data = train_data %>% select(-label)),
        label = train_data$label
    )
    cp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

    full_test <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = test_data %>% select(-label)),
                                       label = test_data$label
    )

    preds <- as.data.frame(
        matrix(predict(cp_model, full_test))
    ) %>%
        dplyr::rename(cp = V1)

    cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
    return(cv_data)
})


plot <- cv_results_raw %>%
    # select(label, cp) %>%
    mutate(
        bin_pred_prob = round(cp / 0.05) * .05,
        distance = case_when(
            event_pass_air_yards < 5 ~ "Short",
            event_pass_air_yards >= 5 & event_pass_air_yards < 15 ~ "Intermediate",
            event_pass_air_yards >= 15 ~ "Deep"
        )
    ) %>%
    group_by(distance, bin_pred_prob) %>%
    mutate(correct = if_else(label == T, 1, 0)) %>%
    summarize(
        n_plays = n(),
        n_complete = length(which(label == 1)),
        bin_actual_prob = n_complete / n_plays
    ) %>%
    ungroup()

ann_text <- data.frame(
    x = c(.25, 0.75), y = c(0.75, 0.25),
    lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

plot %>%
    mutate(distance = fct_relevel(
        distance,
        "Short", "Intermediate", "Deep"
    )) %>%
    filter(n_plays > 10) %>%
    ggplot() +
    geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
    geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
    coord_equal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
        size = "Number of plays",
        x = "Estimated completion percentage",
        y = "Observed completion percentage"
    ) +
    geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 3) +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
    ) +
    facet_wrap(~distance, ncol = 3)

cp_cv_cal_error <- plot %>%
    # ungroup() %>%
    mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
    group_by(distance) %>%
    summarize(
        weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
        n_complete = sum(n_complete, na.rm = TRUE)
    )

round(with(cp_cv_cal_error, weighted.mean(weight_cal_error, n_complete)), 4)



# plot ----
generate_field = function() {
    left_hashes = data.frame(
        x = c(1:99),
        xend = c(1:99),
        y = c((53.33 - 20.00) - 0.66),
        yend = c((53.33 - 20.00))
    )
    right_hashes = data.frame(
        x = c(1:99),
        xend = c(1:99),
        y = c(20),
        yend = c(20+0.66)
    )
    yard_markers = data.frame(
        x=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
        xend = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
        y=c(0),
        yend=c(53.33)
    )

    FarFieldNumbers = data.frame(x= c(10,20,30,40,50,60,70,80,90), y = c(41.33), Number = c(10,20,30,40,50,40,30,20,10))
    CloseFieldNumbers = data.frame(x= c(10,20,30,40,50,60,70,80,90), y = c(12), Number = c(10,20,30,40,50,40,30,20,10))

    ggplot() +
        geom_segment(aes(x=0,y=0,xend=0,yend=53.33)) +
        geom_segment(aes(x=-10,y=0,xend=-10,yend=53.33)) +
        geom_segment(aes(x=100,y=0,xend=100,yend=53.33)) +
        geom_segment(aes(x=110,y=0,xend=110,yend=53.33)) +
        geom_segment(aes(x=-10,y=0,xend=110,yend=0)) +
        geom_segment(aes(x=-10,y=53.33,xend=110,yend=53.33)) +
        geom_segment(data= yard_markers, aes(x=x, xend=xend, y=y, yend=yend)) +
        geom_segment(data = left_hashes, aes(x=x, xend=xend, y=y, yend=yend)) +
        geom_segment(data = right_hashes, aes(x=x, xend=xend, y=y, yend=yend)) +
        # geom_text(data = CloseFieldNumbers, mapping = aes(x,y, label = Number), colour = "#FFFFFF", size = 8,) + ##These are the Numbers on the field
        # geom_text(data = FarFieldNumbers, mapping = aes(x, y, label = Number), colour = "#FFFFFF", size = 8, angle = 180) +
        theme_void()
}


generate_play_plot = function(qb, receiver, LOS, LTG, title, subtitle, defenders = NULL, def_color = NULL) {
    p = generate_field() +
        ggforce::geom_link0(
            data = data.frame(
                x = qb$x,
                y = qb$y,
                xend = receiver$x,
                yend = receiver$y
            ),
            mapping = ggplot2::aes(x = x, y = 53.33 - y, xend = xend, yend = 53.33 - yend),
            size = 1,
            color = "black",
            alpha = 0.5,
            arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "inches"))
        ) +
        ggplot2::geom_point(
            data = qb %>% dplyr::rows_append(receiver),
            mapping = ggplot2::aes(x = x, y = 53.33 - y),
            size = 3,
            color = c("#b3a369", "#003057")
        ) +
        ggplot2::geom_segment(
            data = data.frame(
                x = c(LOS),
                y = c(0),
                xend = c(LOS),
                yend = c(53.33)
            ),
            mapping = ggplot2::aes(x = x, y = 53.33 - y, xend = xend, yend = 53.33 - yend),
            size = 1.5,
            color = "black"
        ) +
        ggplot2::geom_segment(
            data = data.frame(
                x = c(LTG),
                y = c(0),
                xend = c(LTG),
                yend = c(53.33)
            ),
            mapping = ggplot2::aes(x = x, y = 53.33 - y, xend = xend, yend = 53.33 - yend),
            size = 1.5,
            color = "gold"
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle
        ) +
        ggplot2::theme(
            plot.title = ggtext::element_markdown(hjust = 0.5),
            plot.subtitle = ggtext::element_markdown(hjust = 0.5),
            plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, unit = "in")
        )

    if (!is.null(defenders)) {
        p = p +
            ggplot2::geom_point(
                data = defenders,
                mapping = ggplot2::aes(x = x, y = 53.33 - y),
                size = 3,
                color = def_color
            )
    }

    air_yards = sqrt((qb$x - receiver$x)^2 + (qb$y - receiver$y)^2)
    print(glue::glue("Air yards: {air_yards}"))

    defenders %>%
        dplyr::mutate(
            distance_to_receiver = sqrt((x - receiver$x)^2 + (y - receiver$y)^2)
        ) %>%
        arrange(distance_to_receiver) %>%
        print()

    endline_receiver_dist = 110 - receiver$x
    print(glue::glue("to endline: {endline_receiver_dist}"))
    sideline_receiver_dist = min(
        53.33 - receiver$y,
        receiver$y
    )
    print(glue::glue("to sideline: {sideline_receiver_dist}"))

    return(p)
}

generate_play_plot(
    qb = data.frame(
        x = c(52),
        y = c((53.33 - 20.00)-.66)
    ),
    receiver = data.frame(
        x = c(55),
        y = c(20-(.66/2))
    ),
    LOS = 58,
    LTG = 60,
    title = "<strong style='color: #b3a369;'>Haynes King</strong> to <strong style='color: #003057;'>Malik Rutherford</strong>",
    subtitle = "Oct 28, 2023 vs <strong style='color:#7BAFD4'>UNC</strong>: Q2 11:41 - 3rd & 2",
    def_color = "#7BAFD4",
    defenders = data.frame(
        x = c(62, 62, 59),
        y = c(10, 25, 23)
    )
)

ggsave(
    filename = "~/Desktop/unc_play.png",
    plot = ggplot2::last_plot(),
    dpi = 300,
    width = 900,
    height = 500,
    units = "px",
    scale = 2
)

generate_play_plot(
    qb = data.frame(
        x = c(88),
        y = c((53.33 - 20.00) - .66)
    ),
    receiver = data.frame(
        x = c(108),
        y = c((53.33 - 20.00) + 1)
    ),
    LOS = 95.5,
    LTG = 100,
    title = "<strong style='color: #b3a369;'>Haynes King</strong> to <strong style='color: #003057;'>Dominick Blaylock</strong>",
    subtitle = "Sept 9, 2023 vs <strong style='color:#841A2B'>SC State</strong>: Q1 8:31 - 3rd & Goal",
    def_color = "#841A2B",
    defenders = data.frame(
        x = c(107, 106.5, 98.5),
        y = c((53.33 - 20.00) - 0.66, (53.33 - 20.00), 42)
    )
)

ggsave(
    filename = "~/Desktop/scst_play.png",
    plot = ggplot2::last_plot(),
    dpi = 300,
    width = 900,
    height = 500,
    units = "px",
    scale = 2
)

pass_train <- xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = pass_data %>% select(-label, -season)),
    label = pass_data$label
)
final_cp_model <- xgboost::xgboost(params = params, data = pass_train, nrounds = nrounds, verbose = 2)

inputs = data.frame(
    "event_pass_air_yards" = c(13.3416640641263, 20.0687717611218),
    "play_target_separation" = c(5.204700, 1.802776),
    "play_qb_pressure" = c(F, T),
    "endline_receiver_dist" = c(55, 2),
    "sideline_receiver_dist" = c(19.67, 19)
)
input_test <- xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = inputs)
)
outputs <- as.data.frame(
    matrix(predict(final_cp_model, input_test))
) %>%
    dplyr::rename(cp = V1)

results <- bind_cols(inputs, outputs)

save_crop_gt <- function(gt_obj, file, whitespace = 50) {
    gtExtras::gtsave_extra(gt_obj, paste0("~/Desktop/", file), zoom = 2)

    magick::image_read(paste0("~/Desktop/", file)) |>
        magick::image_trim() |>
        magick::image_border("white", glue::glue('{whitespace}x{whitespace}')) |>
        magick::image_write(paste0("~/Desktop/", file))
}


inputs %>%
    dplyr::mutate(
        `Play #` = dplyr::row_number()
    ) %>%
    dplyr::relocate(`Play #`) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_label(
        "event_pass_air_yards" = "Air Yards",
        "play_target_separation" = "Yds Separation from Defender",
        "play_qb_pressure" = "QB under Pressure?",
        "endline_receiver_dist" = "Yds to Endline",
        "sideline_receiver_dist" = "Yds to Sideline"
    ) %>%
    gt::cols_align(
        columns = -`Play #`,
        align = "center"
    ) %>%
    gt::cols_align(
        columns = `Play #`,
        align = "left"
    ) %>%
    gt::fmt_number(
        columns = -c(play_qb_pressure, `Play #`),
        decimals = 2
    ) %>%
    gt::tab_style(
        locations = gt::cells_body(
            columns = `Play #`
        ),
        style = gt::cell_text(weight = "bold")
    ) %>%
    save_crop_gt("example_inputs.png")

results %>%
    dplyr::mutate(
        `Play #` = dplyr::row_number(),
        play_qb_pressure = (play_qb_pressure == 1)
    ) %>%
    dplyr::relocate(`Play #`) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_label(
        "event_pass_air_yards" = "Air Yards",
        "play_target_separation" = "Yds Separation from Defender",
        "play_qb_pressure" = "QB under Pressure?",
        "endline_receiver_dist" = "Yds to Endline",
        "sideline_receiver_dist" = "Yds to Sideline",
        "cp" = "Exp Comp %"
    ) %>%
    gt::cols_align(
        columns = -`Play #`,
        align = "center"
    ) %>%
    gt::cols_align(
        columns = `Play #`,
        align = "left"
    ) %>%
    gt::cols_align(
        columns = `cp`,
        align = "right"
    ) %>%
    gt::fmt_number(
        columns = -c(play_qb_pressure, `Play #`, cp),
        decimals = 2
    ) %>%
    gt::fmt_percent(
        columns = cp,
        decimals = 1
    ) %>%
    gt::tab_style(
        locations = gt::cells_body(
            columns = `Play #`
        ),
        style = gt::cell_text(weight = "bold")
    ) %>%
    gtExtras::gt_color_rows(cp, palette = "ggsci::blue_material", domain = 0:1) %>%
    save_crop_gt("example_outputs.png")
