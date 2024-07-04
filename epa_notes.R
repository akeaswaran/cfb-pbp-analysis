library(tidyverse)
library(dplyr)
library(glue)
library(stringr)
library(gt)
library(gtExtras)
library(cbbdata)
library(cbbplotR)

# valid_fbs_teams <- cfbfastR::load_cfb_teams() %>%
#     filter(classification == 'fbs') %>%
#     select(
#         team_id,
#         school,
#         abbreviation
#     )

games = purrr::map_dfr(2014:2023, function(x) cfbfastR::cfbd_game_info(x))
plays = cfbfastR::load_cfb_pbp(2014:2023)


fbs_game_info = games %>%
    dplyr::filter(
        home_division == "fbs"
        & away_division == "fbs"
    )

fbs_games = plays %>%
    dplyr::filter(game_id %in% fbs_game_info$game_id) %>%
    dplyr::filter(pass == 1 | rush == 1) %>%
    dplyr::group_by(season = year, game_id, pos_team) %>%
    dplyr::summarize(
        Plays = dplyr::n(),
        `Yds` = sum(yards_gained, na.rm = T),
        `EPA/Play` = mean(EPA, na.rm = T),
        `EPA` = sum(EPA, na.rm = T),
        `Yds/Play` = mean(yards_gained, na.rm = T),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(fbs_game_info %>% dplyr::select(game_id, home_team, pos_score = home_points), by = c("game_id", "pos_team" = "home_team")) %>%
    dplyr::left_join(fbs_game_info %>% dplyr::select(game_id, away_team, pos_score = away_points), by = c("game_id", "pos_team" = "away_team")) %>%
    tidyr::unite(col = "pos_score", pos_score.x, pos_score.y, remove = T, na.rm = T) %>%
    dplyr::mutate(
        pos_score = as.integer(pos_score),
    ) %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(
        `EPA/Play Diff` = dplyr::case_when(
            dplyr::row_number() == 1 ~ dplyr::first(`EPA/Play`) - dplyr::last(`EPA/Play`),
            dplyr::row_number() == 2 ~ dplyr::last(`EPA/Play`) - dplyr::first(`EPA/Play`),
        ),
        `EPA Diff` = dplyr::case_when(
            dplyr::row_number() == 1 ~ dplyr::first(`EPA`) - dplyr::last(`EPA`),
            dplyr::row_number() == 2 ~ dplyr::last(`EPA`) - dplyr::first(`EPA`),
        ),
        `Pts Diff` = dplyr::case_when(
            dplyr::row_number() == 1 ~ dplyr::first(`pos_score`) - dplyr::last(`pos_score`),
            dplyr::row_number() == 2 ~ dplyr::last(`pos_score`) - dplyr::first(`pos_score`),
        ),
        `Win` = `Pts Diff` > 0
    ) %>%
    dplyr::ungroup()

ggplot2::ggplot(fbs_games %>% dplyr::filter(abs(`EPA Diff`) < 50), ggplot2::aes(x = `EPA/Play Diff`, y = `Pts Diff`)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_smooth(method = "lm") +
    ggpubr::stat_cor(ggplot2::aes(label = paste(ggplot2::after_stat(rr.label), ggplot2::after_stat(r.label), sep = "~")), label.y = 75, label.sep = "\\n") + #this means at 35th unit in the y axis, the r squared and p value will be shown
    # ggpubr::stat_regline_equation(label.y = 70) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
        title = "Exploring the Relationship between EPA and Points",
        subtitle = "FBS games, 2014-2023",
        caption = "EPA Diff Range: -50 to +50.\nData from cfbfastR (@cfbfastR).\nChart made by Akshay Easwaran (@akeaswaran). "
    )
ggsave(filename = "~/Desktop/epa_play_pts_14_24.png", width = 1000, height = 800, units = "px", scale = 3, dpi = 300, limitsize = F)

ggplot2::ggplot(fbs_games %>% dplyr::filter(abs(`EPA Diff`) < 50), ggplot2::aes(x = `EPA Diff`, y = `Pts Diff`)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_smooth(method = "lm") +
    ggpubr::stat_cor(ggplot2::aes(label = paste(ggplot2::after_stat(rr.label), ggplot2::after_stat(r.label), sep = "~")), label.y = 75, label.sep = "\\n") + #this means at 35th unit in the y axis, the r squared and p value will be shown
    # ggpubr::stat_regline_equation(label.y = 70) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
        title = "Exploring the Relationship between EPA and Points",
        subtitle = "FBS games, 2014-2023",
        caption = "EPA Diff Range: -50 to +50.\nData from cfbfastR (@cfbfastR).\nChart made by Akshay Easwaran (@akeaswaran). "
    )
ggsave(filename = "~/Desktop/epa_pts_14_24.png", width = 1000, height = 800, units = "px", scale = 3, dpi = 300, limitsize = F)


fbs_teams_23 = fbs_plays %>%
    dplyr::filter(season == 2023 & (pass == 1 | rush == 1)) %>%
    dplyr::group_by(Team = pos_team) %>%
    dplyr::summarize(
        Plays = dplyr::n(),
        `Yds/Game` = sum(yards_gained, na.rm = T) / length(unique(game_id)),
        `EPA/Game` = sum(EPA, na.rm = T) / length(unique(game_id)),
        `Yds/Play` = mean(yards_gained, na.rm = T),
        `EPA/Play` = mean(EPA, na.rm = T),
    ) %>%
    dplyr::ungroup()


top_epa_gm = fbs_teams_23 %>%
    dplyr::arrange(-`EPA/Game`) %>%
    cbbplotR::gt_cbb_teams(Team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team, team, Plays, `Yds/Game`, `EPA/Game`) %>%
    head(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**2023 Offensive EPA/Game Rankings**"),
        # subtitle = gt::md("Scrimmage plays only.")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`EPA/Game`, `Yds/Game`),
        decimals = 2,
    ) %>%
    # gt::fmt_percent(
    #     columns = pct_convert,
    #     decimals = 1
    # ) %>%
    gtExtras::gt_color_rows(`EPA/Game`, palette = "ggsci::green_material", direction = 1, domain = fbs_teams_23$`EPA/Game`) |>
    gtExtras::gt_color_rows(`Yds/Game`, palette = "ggsci::blue_material", direction = 1, domain = fbs_teams_23$`Yds/Game`) |> # set green or red palette
    # gt::tab_style(
    #     style = list(
    #         gt::cell_text(weight = "bold")
    #     ),
    #     locations = gt::cells_body(
    #         columns = c("season"),
    #     )
    # ) %>%
    gt::tab_options(
        heading.align = "left"
    ) %>%
    # gt::cols_label(
    #     "season" ~ "Season",
    #     "count" ~  "4th Downs",
    #     "n_convert" ~ "Chose to Go",
    #     "pct_convert" ~ "Chose to Go %"
    # ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))


bottom_epa_gm = fbs_teams_23 %>%
    dplyr::arrange(-`EPA/Game`) %>%
    cbbplotR::gt_cbb_teams(Team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team, team, Plays, `Yds/Game`, `EPA/Game`) %>%
    tail(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**2023 Offensive EPA/Game Rankings**"),
        # subtitle = gt::md("Scrimmage plays only.")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`EPA/Game`, `Yds/Game`),
        decimals = 2,
    ) %>%
    # gt::fmt_percent(
    #     columns = pct_convert,
    #     decimals = 1
    # ) %>%
    gtExtras::gt_color_rows(`EPA/Game`, palette = "ggsci::green_material", direction = 1, domain = fbs_teams_23$`EPA/Game`) |>
    gtExtras::gt_color_rows(`Yds/Game`, palette = "ggsci::blue_material", direction = 1, domain = fbs_teams_23$`Yds/Game`) |># set green or red palette
    # gt::tab_style(
    #     style = list(
    #         gt::cell_text(weight = "bold")
    #     ),
    #     locations = gt::cells_body(
    #         columns = c("season"),
    #     )
    # ) %>%
    gt::tab_options(
        heading.align = "left"
    ) %>%
    # gt::cols_label(
    #     "season" ~ "Season",
    #     "count" ~  "4th Downs",
    #     "n_convert" ~ "Chose to Go",
    #     "pct_convert" ~ "Chose to Go %"
    # ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))

top_epa_play = fbs_teams_23 %>%
    dplyr::arrange(-`EPA/Play`) %>%
    cbbplotR::gt_cbb_teams(Team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team, team, Plays, `Yds/Play`, `EPA/Play`) %>%
    head(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**2023 Offensive EPA/Play Rankings**"),
        # subtitle = gt::md("Scrimmage plays only.")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`EPA/Play`, `Yds/Play`),
        decimals = 2,
    ) %>%
    # gt::fmt_percent(
    #     columns = pct_convert,
    #     decimals = 1
    # ) %>%
    gtExtras::gt_color_rows(`EPA/Play`, palette = "ggsci::green_material", direction = 1, domain = fbs_teams_23$`EPA/Play`) |>
    gtExtras::gt_color_rows(`Yds/Play`, palette = "ggsci::blue_material", direction = 1, domain = fbs_teams_23$`Yds/Play`) |># set green or red palette
    # gt::tab_style(
    #     style = list(
    #         gt::cell_text(weight = "bold")
    #     ),
    #     locations = gt::cells_body(
    #         columns = c("season"),
    #     )
    # ) %>%
    gt::tab_options(
        heading.align = "left"
    ) %>%
    # gt::cols_label(
    #     "season" ~ "Season",
    #     "count" ~  "4th Downs",
    #     "n_convert" ~ "Chose to Go",
    #     "pct_convert" ~ "Chose to Go %"
    # ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))


bottom_epa_play = fbs_teams_23 %>%
    dplyr::arrange(-`EPA/Play`) %>%
    cbbplotR::gt_cbb_teams(Team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team, team, Plays, `Yds/Play`, `EPA/Play`) %>%
    tail(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**2023 Offensive EPA/Play Rankings**"),
        # subtitle = gt::md("Scrimmage plays only.")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`EPA/Play`, `Yds/Play`),
        decimals = 2,
    ) %>%
    gtExtras::gt_color_rows(`EPA/Play`, palette = "ggsci::green_material", direction = 1, domain = fbs_teams_23$`EPA/Play`) |>
    gtExtras::gt_color_rows(`Yds/Play`, palette = "ggsci::blue_material", direction = 1, domain = fbs_teams_23$`Yds/Play`) |>
    gt::tab_options(
        heading.align = "left"
    ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))

save_crop_gt <- function(gt_obj, file, whitespace = 50) {
    gtExtras::gtsave_extra(gt_obj, paste0("~/Desktop/", file), zoom = 2)

    magick::image_read(paste0("~/Desktop/", file)) |>
        magick::image_trim() |>
        magick::image_border("white", glue::glue('{whitespace}x{whitespace}')) |>
        magick::image_write(paste0("~/Desktop/", file))
}

top_epa_gm %>%
    save_crop_gt("23_top_epa_gm.png")
bottom_epa_gm %>%
    save_crop_gt("23_bottom_epa_gm.png")
top_epa_play %>%
    save_crop_gt("23_top_epa_play.png")
bottom_epa_play %>%
    save_crop_gt("23_bottom_epa_play.png")


fbs_games %>%
    dplyr::summarize(
        p90 = quantile(`EPA/Play`, probs = .9),
        p75 = quantile(`EPA/Play`, probs = .75),
        p50 = quantile(`EPA/Play`, probs = .5),
        p25 = quantile(`EPA/Play`, probs = .25),
        p10 = quantile(`EPA/Play`, probs = .1)
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::mutate(
        pctl_perf = dplyr::case_when(
            name == "p90" ~ "90th",
            name == "p75" ~ "75th",
            name == "p50" ~ "50th",
            name == "p25" ~ "25th",
            name == "p10" ~ "10th"
        ),
        name = dplyr::case_when(
            pctl_perf == "90th" ~ "Excellent",
            pctl_perf == "75th" ~ "Good",
            pctl_perf == "50th" ~ "Average",
            pctl_perf == "25th" ~ "Bad",
            pctl_perf == "10th" ~ "Very Bad"
        )
    ) %>%
    dplyr::relocate(pctl_perf, .after = name) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**Offensive EPA/Play Tiers**"),
        subtitle = gt::md("Based on team performances in EPA/play in FBS games from 2014 to 2023.")
    ) %>%
    gt::cols_align(columns = c("name"), align = "left") %>%
    gt::cols_align(columns = -name, align = "center") %>%
    gt::cols_align(columns = value, align = "right") %>%
    gt::fmt_number(
        columns = c(`value`),
        decimals = 2,
    ) %>%
    gt::cols_label(
        "pctl_perf" = "%tile"
    ) %>%
    gtExtras::gt_color_rows(value, palette = "ggsci::green_material", direction = 1) %>%
    gt::tab_source_note(source_note = gt::md(
        "Scrimmage plays only. Data from cfbfastR. Table assembled by @akeaswaran."
    )) %>%
    save_crop_gt("epa_tiers.png")

fbs_plays %>%
    dplyr::filter(
        (pass == 1 | rush == 1)
        & down != 0
        & down != 5
        & distance <= 15
        & wp_before >= 0.1
        & wp_before <= 0.9
    ) %>%
    dplyr::mutate(down = as.factor(down)) %>%
    dplyr::group_by(down, distance) %>%
    dplyr::summarize(
        count = dplyr::n(),
        dropback_rate = mean(`pass`, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = distance, y = dropback_rate, color = down)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "loess", se = F)


fbs_plays %>%
    dplyr::filter(
        pass == 1
        & down != 0
        & down != 5
        & distance <= 15
        & wp_before >= 0.1
        & wp_before <= 0.9
    ) %>%
    dplyr::mutate(down = as.factor(down)) %>%
    dplyr::group_by(down, distance) %>%
    dplyr::summarize(
        count = dplyr::n(),
        sack_rate = mean(`sack`, na.rm = T),
        throwaway_rate = mean(
            is.na(incompletion_player)
            & is.na(receiver_player_name)
            & is.na(pass_breakup_player)
            & is.na(interception_player)
            & is.na(fumble_player_name)
            & (sack == 0 | is.na(sack)),
            na.rm = T
        )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(sack_rate, throwaway_rate)) %>%
    ggplot2::ggplot(ggplot2::aes(x = distance, y = value, color = name)) +
    ggplot2::geom_point(mapping = ggplot2::aes(size = count)) +
    ggplot2::geom_smooth(method = "loess", se = F) +
    ggplot2::facet_wrap(~ down)


epa_play_model = lm(`Win` ~ `EPA Diff`, fbs_games)
summary(epa_play_model)

cor(fbs_games$`EPA Diff`, fbs_games$`Win`)
cor(fbs_games$`EPA Diff`, fbs_games$`Win`)^2


exp_games_23 = fbs_games %>%
    dplyr::filter(season == 2023) %>%
    dplyr::mutate(
        exp_win = stats::predict(epa_play_model, .),
        exp_win = dplyr::case_when(
            exp_win < 0 ~ 0,
            exp_win > 1 ~ 1,
            .default = exp_win
        )
    )

exp_games_23 %>%
    dplyr::filter(pos_team == "Georgia Tech") %>%
    dplyr::select(season, game_id, team = pos_team, exp_win) %>%
    dplyr::left_join(fbs_game_info %>% dplyr::select(game_id, start_date, home_team, away_team, home_points, away_points), by = c("game_id", "team" = "home_team")) %>%
    dplyr::left_join(fbs_game_info %>% dplyr::select(game_id, start_date, home_team, away_team, home_points, away_points), by = c("game_id", "team" = "away_team")) %>%
    tidyr::unite(col = "start_date", start_date.x, start_date.y, na.rm = T) %>%
    tidyr::unite(col = "home_points", home_points.x, home_points.y, na.rm = T) %>%
    tidyr::unite(col = "away_points", away_points.x, away_points.y, na.rm = T) %>%
    dplyr::mutate(
        team_points = dplyr::case_when(
            !is.na(away_team) ~ home_points,
            !is.na(home_team) ~ away_points,
        ),
        opponent_points = dplyr::case_when(
            !is.na(away_team) ~ away_points,
            !is.na(home_team) ~ home_points,
            # .default = team
        ),
    ) %>%
    dplyr::select(-home_points, -away_points) %>%
    tidyr::unite(col = "opponent", home_team, away_team, na.rm = T) %>%
    cbbplotR::gt_cbb_teams(team, logo_column = "team") %>%
    cbbplotR::gt_cbb_teams(opponent, logo_column = "opponent") %>%
    dplyr::arrange(season, start_date) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_hide(
        columns = c(season, game_id)
    ) %>%
    gt::fmt_markdown(team) %>%
    gt::fmt_markdown(opponent) %>%
    gt::fmt_date(
        columns = c(start_date),
        # tz = "America/New_York"
    ) %>%
    gt::cols_align(
        columns = c(start_date, team, opponent),
        align = "left"
    ) %>%
    gt::cols_align(
        columns = dplyr::ends_with("points"),
        align = "center"
    ) %>%
    gt::cols_align(
        columns = exp_win,
        align = "right"
    ) %>%
    gt::fmt_percent(
        columns = exp_win,
        decimals = 0
    ) %>%
    gt::cols_move_to_end(exp_win) %>%
    gt::cols_move_to_start(start_date) %>%
    gt::cols_label(
        start_date = "Date",
        exp_win = "Exp Win %",
        team_points = "Team Pts",
        opponent_points = "Opp Pts"
    ) %>%
    gt::cols_hide(team) %>%
    gtExtras::gt_color_rows(exp_win, domain = 0:1, palette = "ggsci::blue_material") %>%
    gt::tab_style(
        locations = gt::cells_body(
            columns = team_points,
            rows = team_points > opponent_points
        ),
        style = gt::cell_fill(color = "#71B367")
    ) %>%
    gt::tab_style(
        locations = gt::cells_body(
            columns = team_points,
            rows = team_points > opponent_points
        ),
        style = gt::cell_text(weight = "bold")
    ) %>%
    gt::tab_header(title = gt::html(
        cbbplotR::gt_cbb_logo_title(title = 'Expected vs Actual Wins',
                          subtitle = "<span style='color: #b3a369'>Georgia Tech</span> (2023) - FBS games only",
                          type = 'team',
                          value = 'Georgia Tech',
                          logo_height = 45)
    )) %>%
    gt::tab_source_note(source_note = gt::md(
        "Expected wins model based on per-game EPA differential. Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))


exp_teams_23 = exp_games_23 %>%
    dplyr::group_by(season, pos_team) %>%
    dplyr::summarize(
        expected_wins = sum(exp_win, na.rm = T),
        actual_wins = sum(Win, na.rm = T),
        diff = actual_wins - expected_wins
    ) %>%
    dplyr::ungroup()



top_exp_ranks = exp_teams_23 %>%
    arrange(-diff) %>%
    # View()
    cbbplotR::gt_cbb_teams(pos_team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team = pos_team, team, `Wins` = actual_wins, `Expected` = expected_wins, `+/-` = diff) %>%
    head(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**EPA Overperformance Rankings**"),
        subtitle = gt::md("2023 season, FBS vs FBS games only")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`Expected`, `+/-`),
        decimals = 1,
    ) %>%
    gt::fmt_number(
        columns = c(`+/-`),
        decimals = 1,
        force_sign = T
    ) %>%
    gtExtras::gt_color_rows(`Expected`, palette = "ggsci::green_material", direction = 1, domain = exp_teams_23$`expected_wins`) |>
    gtExtras::gt_color_rows(`+/-`, palette = "ggsci::blue_material", direction = 1, domain = exp_teams_23$`diff`) |>
    gt::tab_options(
        heading.align = "left"
    ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Model based on per-game EPA differential. Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))

bottom_exp_ranks = exp_teams_23 %>%
    arrange(diff) %>%
    # View()
    cbbplotR::gt_cbb_teams(pos_team) %>%
    dplyr::mutate(
        Rk = dplyr::row_number()
    ) %>%
    dplyr::select(Rk, Team = pos_team, team, `Wins` = actual_wins, `Expected` = expected_wins, `+/-` = diff) %>%
    head(10) %>%
    gt::gt() %>%
    gt::fmt_markdown(team) %>%
    gt::cols_hide(Team) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(
        title = gt::md("**EPA Underperformance Rankings**"),
        subtitle = gt::md("2023 season, FBS vs FBS games only")
    ) %>%
    gt::cols_align(columns = c("team", "Rk"), align = "left") %>%
    gt::cols_align(columns = -team, align = "center") %>%
    gt::fmt_number(
        columns = c(`Expected`, `+/-`),
        decimals = 1,
    ) %>%
    gt::fmt_number(
        columns = c(`+/-`),
        decimals = 1,
        force_sign = T
    ) %>%
    gtExtras::gt_color_rows(`Expected`, palette = "ggsci::green_material", direction = 1, domain = exp_teams_23$`expected_wins`) |>
    gtExtras::gt_color_rows(`+/-`, palette = "ggsci::blue_material", direction = 1, domain = exp_teams_23$`diff`) |>
    gt::tab_options(
        heading.align = "left"
    ) %>%
    gt::tab_source_note(source_note = gt::md(
        "Model based on per-game EPA differential. Scrimmage plays only. Data from cfbfastR. Logos from cbbplotR.<br/>Table assembled by @akeaswaran."
    ))

top_exp_ranks %>%
    save_crop_gt("23_top_exp_ranks.png")
bottom_exp_ranks %>%
    save_crop_gt("23_bottom_exp_ranks.png")
