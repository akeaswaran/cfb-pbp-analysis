years -> 2010:2020

update_file <- function(df, path, force) {
    if (!file.exists(path) || force) {
        write.csv(df, path, row.names=FALSE)
    }
}

for (yr in years) {
    update_file(cfbfastR::cfbd_drives(yr), glue::glue("data/drives/{yr}.csv"), (yr == 2021))
    update_file(cfbfastR::cfbd_game_info(yr), glue::glue("data/games/{yr}.csv"), (yr == 2021))
    update_file(cfbfastR::cfbd_betting_lines(yr), glue::glue("data/lines/{yr}.csv"), (yr == 2021))
    update_file(cfbfastR::cfbd_rankings(yr), glue::glue("data/rankings/{yr}.csv"), (yr == 2021))
    update_file(cfbfastR::cfbd_recruiting_team(year=yr), glue::glue("data/recruiting/{yr}.csv"), (yr == 2021))
    update_file(cfbfastR::cfbd_team_info(year=yr), glue::glue("data/teams/{yr}.csv"), (yr == 2021))
    pbp_data <- data.frame()
    for (wk in 1:15) {
        progressr::with_progress({
            future::plan("multisession")
            tmp = cfbfastR::cfbd_pbp_data(yr, week = wk)
            pbp_data <- rbind(pbp_data, tmp)

        })
        update_file(pbp_data, glue::glue("data/pbp/{yr}.csv"), (yr == 2021))
    }
}
