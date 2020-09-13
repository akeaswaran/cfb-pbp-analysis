library(cfbscrapR)
library(stringr)
library(data.table)
library(dplyr)

pbp <- data.frame()

for (i in 1:15) {
    tmp_df <- cfb_pbp_data(2019, week=i, team="Georgia Tech", epa_wpa=TRUE)
    if (length(tmp_df) > 0 && nrow(tmp_df) > 0) {
        pbp <- rbind(pbp, tmp_df)
    }
}

pbp <- pbp[
    !((pbp$down == 0)
    | ((abs(pbp$score_diff) >= 38) & (pbp$period == 2))
    | ((abs(pbp$score_diff) >= 28) & (pbp$period == 3))
    | ((abs(pbp$score_diff) >= 22) & (pbp$period == 4))),
]

findReceiverData <- function (wrName, team) {
    # tmp_df <- pbp[
    #     (grepl(wrName, pbp$play_text) = TRUE
    #     # && pbp$play_type %in% allow_types
    #     && pbp$offense_play == team),
    # ]
    team_plays <- pbp %>% filter(grepl(team, offense_play))
    pass_plays <- team_plays %>% filter(grepl("Pass", play_type))
    player_plays <- pass_plays %>% filter(grepl(wrName, play_text))
    return(player_plays)
}

wr_data <- findReceiverData("Ahmarean Brown", "Georgia Tech")
print(paste0("EPA/Target: ",sum(wr_data$EPA) / nrow(wr_data)))
print(paste0("GT EPA/Play: ", mean(pbp[which(!is.na(pbp$EPA)),]$EPA)))
