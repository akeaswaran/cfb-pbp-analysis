library(cfbfastR)
library(dplyr)
library(glue)
library(stringr)
library(ggplot2)

max_season <- cfbfastR:::most_recent_cfb_season()
seasons <- 2014:max_season
valid_fbs_teams <- cfbfastR::load_cfb_teams() %>%
    filter(classification == 'fbs') %>%
    select(
        team_id,
        school,
        abbreviation
    )

plays = cfbfastR::load_cfb_pbp(seasons)

fbs_plays = plays %>%
    dplyr::filter(home %in% valid_fbs_teams$school & away %in% valid_fbs_teams$school) %>%
    # dplyr::filter(game_id == first(fbs_plays$game_id)) %>%
    dplyr::group_by(game_id, half) %>%
    dplyr::mutate(
        scoring_drive = dplyr::case_when(
            scoring_play == 1 ~ drive_id,
            .default = NA_integer_
        ),
        scoring_team = dplyr::case_when(
            scoring_play & offense_score_play ~ pos_team,
            scoring_play & defense_score_play ~ def_pos_team,
            TRUE ~ NA_character_
        ),
        scoring_type = dplyr::case_when(
            scoring_play == 1 ~ play_type,
            # scoring_play & defense_score_play ~ def_pos_team,
            TRUE ~ NA_character_
        )
    ) %>%
    tidyr::fill(scoring_team, .direction = "up") %>%
    tidyr::fill(scoring_type, .direction = "up") %>%
    tidyr::fill(scoring_drive, .direction = "up") %>%
    dplyr::mutate(
        Next_Score_Half = dplyr::case_when(
            pos_team == scoring_team & grepl("Touchdown", scoring_type) ~ "Touchdown",
            pos_team != scoring_team & grepl("Touchdown", scoring_type) ~ "Opp_Touchdown",

            pos_team == scoring_team & grepl("Field Goal Good", scoring_type) ~ "Field_Goal",
            pos_team != scoring_team & grepl("Field Goal Good", scoring_type) ~ "Opp_Field_Goal",

            pos_team == scoring_team & grepl("Safety", scoring_type) ~ "Safety",
            pos_team != scoring_team & grepl("Safety", scoring_type) ~ "Opp_Safety",

            .default = "No_Score"
        ),
        scoring_drive = dplyr::case_when(
            Next_Score_Half == "No_Score" ~ drive_id,
            .default = scoring_drive
        )
    ) %>%
    dplyr::ungroup()



plays_set = fbs_plays %>%
    dplyr::mutate(
        special_teams_play = (fg_inds == 1 | punt == 1 | kickoff_play == 1),
        half_seconds_remaining = dplyr::case_when(
            period > 4 ~ 0,
            period %in% c(1, 3) ~ (15 * 60) + ((clock.minutes * 60) + clock.seconds),
            .default = ((clock.minutes * 60) + clock.seconds)
        ),
        game_seconds_remaining = dplyr::case_when(
            period > 4 ~ 0,
            period %in% c(1, 2) ~ (2 * 15 * 60) + half_seconds_remaining,
            .default = half_seconds_remaining
        ),
    ) %>%
    dplyr::filter(
        !is.na(Next_Score_Half)
        & Next_Score_Half %in% c(
            "Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
            "Field_Goal", "No_Score", "Safety", "Touchdown"
        )
        & (pass == 1 | rush == 1 | special_teams_play)
        & !is.na(down)
        & !is.na(game_seconds_remaining)
    ) %>%
    dplyr::mutate(
        down1 = dplyr::if_else(down == 1, 1, 0),
        down2 = dplyr::if_else(down == 2, 1, 0),
        down3 = dplyr::if_else(down == 3, 1, 0),
        down4 = dplyr::if_else(down == 4, 1, 0),

        is_home = (pos_team == home),

        label = case_when(
            Next_Score_Half == "Touchdown" ~ 0,
            Next_Score_Half == "Opp_Touchdown" ~ 1,

            Next_Score_Half == "Field_Goal" ~ 2,
            Next_Score_Half == "Opp_Field_Goal" ~ 3,

            Next_Score_Half == "Safety" ~ 4,
            Next_Score_Half == "Opp_Safety" ~ 5,

            Next_Score_Half == "No_Score" ~ 6
        ),

        # use nflscrapR weights
        # Calculate the drive difference between the next score drive and the
        # current play drive:
        Drive_Score_Dist = Drive_Score_Half - drive_id,
        # Create a weight column based on difference in drives between play and next score:
        Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
            (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
        # Create a weight column based on score differential:
        ScoreDiff_W = (max(abs(score_differential), na.rm=T) - abs(score_differential)) /
            (max(abs(score_differential), na.rm=T) - min(abs(score_differential), na.rm=T)),
        # Add these weights together and scale again:
        Total_W = Drive_Score_Dist_W + ScoreDiff_W,
        Total_W_Scaled = (Total_W - min(Total_W, na.rm=T)) /
            (max(Total_W, na.rm=T) - min(Total_W, na.rm=T))
    ) %>%
    filter(
        !is.na(pos_team_timeouts)
        & !is.na(def_pos_team_timeouts)
        & !is.na(yards_to_goal)
    ) %>%
    select(
        label,

        season = year,
        half_seconds_remaining,

        down1, down2, down3, down4,
        distance,
        yards_to_goal,

        is_home,
        pos_team_timeouts,
        def_pos_team_timeouts,

        Total_W_Scaled
    )

remove(fbs_plays, plays)

nrounds <- 525
params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 7,
    eta = 0.025,
    gamma = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1
)


cv_results_raw <- purrr::map_dfr(seasons, function(x) {
    test_data <- plays_set %>%
        dplyr::filter(season == x) %>%
        dplyr::select(-season)
    train_data <- plays_set %>%
        dplyr::filter(season != x) %>%
        dplyr::select(-season)

    full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-label, -Total_W_Scaled)),
                                       label = train_data$label, weight = train_data$Total_W_Scaled
    )
    ep_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2, print_every_n = 10L)

    full_test =  xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = test_data %>% dplyr::select(-label, -Total_W_Scaled)),
                                      label = test_data$label, weight = test_data$Total_W_Scaled
    )

    preds <- as.data.frame(
        matrix(predict(ep_model, full_test), ncol = 7, byrow = TRUE)
    )
    colnames(preds) <- c(
        "Touchdown", "Opp_Touchdown", "Field_Goal", "Opp_Field_Goal",
        "Safety", "Opp_Safety", "No_Score"
    )

    cv_data <- dplyr::bind_cols(test_data, preds) %>% dplyr::mutate(season = x)
    return(cv_data)
})

# get the BINS for the calibration plot
plot <- cv_results_raw %>%
    select(Touchdown, Opp_Touchdown, Field_Goal, Opp_Field_Goal, Safety, Opp_Safety, No_Score, label) %>%
    tidyr::pivot_longer(-label, names_to = "type", values_to = "pred_prob") %>%
    mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
    mutate(outcome = case_when(
        label == 0 ~ "Touchdown",
        label == 1 ~ "Opp_Touchdown",
        label == 2 ~ "Field_Goal",
        label == 3 ~ "Opp_Field_Goal",
        label == 4 ~ "Safety",
        label == 5 ~ "Opp_Safety",
        label == 6 ~ "No_Score"
    )) %>%
    group_by(type, bin_pred_prob) %>%
    mutate(correct = if_else(outcome == type, 1, 0)) %>%
    summarize(
        n_plays = n(),
        n_outcome = sum(correct),
        bin_actual_prob = n_outcome / n_plays
    ) %>%
    ungroup()

ann_text <- data.frame(
    x = c(.25, 0.75), y = c(0.75, 0.25),
    lab = c("More times\nthan expected", "Fewer times\nthan expected"),
    next_score_type = factor("No Score (0)")
)
plot %>%
    # about .75M plays in total
    # filter(n_plays >= 50) %>%
    mutate(
        type = forcats::fct_relevel(
            type,
            "Opp_Safety", "Opp_Field_Goal",
            "Opp_Touchdown", "No_Score", "Safety",
            "Field_Goal", "Touchdown"
        ),
        type = forcats::fct_recode(type,
                                   "-Field Goal (-3)" = "Opp_Field_Goal",
                                   "-Safety (-2)" = "Opp_Safety",
                                   "-Touchdown (-7)" = "Opp_Touchdown",
                                   "Field Goal (3)" = "Field_Goal",
                                   "No Score (0)" = "No_Score",
                                   "Touchdown (7)" = "Touchdown",
                                   "Safety (2)" = "Safety"
        )
    ) %>%
    ggplot() +
    geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
    geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
    coord_equal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
        size = "Number of plays",
        x = "Estimated next score probability",
        y = "Observed next score probability"
    ) +
    geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
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
        legend.position = c(1, .05), legend.justification = c(1, 0)
    ) +
    facet_wrap(~type, ncol = 4)

cv_cal_error <- plot %>%
    mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
    group_by(type) %>%
    summarize(
        weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
        n_scoring_event = sum(n_outcome, na.rm = TRUE)
    )

glue::glue(
    "
--CALIBRATION ERROR--

cfbfastR:
{round(with(cv_cal_error, weighted.mean(weight_cal_error, n_scoring_event)), 4)}
"
)

cv_results = cv_results_raw %>%
    dplyr::mutate(
        EP = (No_Score * 0)
        + (Field_Goal * 3)
        + (Touchdown * 7)
        + (Safety * 2)
        + (Opp_Field_Goal * -3)
        + (Opp_Safety * -2)
        + (Opp_Touchdown * -7)
    )

right_field = cv_results %>%
    dplyr::filter(down1 == 1 & distance == 10 & yards_to_goal == 75 & is_home & half_seconds_remaining == 1800 & pos_team_timeouts == 3 & def_pos_team_timeouts == 3) %>%
    dplyr::select(half_seconds_remaining, dplyr::starts_with("down"), distance, yards_to_goal, is_home, pos_team_timeouts, def_pos_team_timeouts, EP) %>%
    dplyr::mutate(
        Location = "Right Field"
    ) %>%
    head(1)

left_stadium = cv_results %>%
    dplyr::filter(down4 == 1 & distance == 1 & yards_to_goal == 25 & is_home & half_seconds_remaining == 20) %>%
    dplyr::select(half_seconds_remaining, dplyr::starts_with("down"), distance, yards_to_goal, is_home, pos_team_timeouts, def_pos_team_timeouts, EP) %>%
    dplyr::mutate(
        Location = "Left Stadium"
    )

dplyr::bind_rows(right_field, left_stadium) %>%
    dplyr::mutate(
        down = dplyr::case_when(
            down1 == 1 ~ 1,
            down2 == 1 ~ 2,
            down3 == 1 ~ 3,
            down4 == 1 ~ 4,
        )
    ) %>%
    dplyr::select(
        Location,
        `Time Remaining in Half` = half_seconds_remaining,
        Down = down,
        Distance = distance,
        `Yards to Goal` = yards_to_goal,
        `Offense Timeouts` = pos_team_timeouts,
        `Defense Timeouts` = def_pos_team_timeouts,
        `Is Offense Home Team?` = is_home,
        `Expected Points` = EP
    ) %>% View()

