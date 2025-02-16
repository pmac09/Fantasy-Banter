library(fitzRoy)

# import supercoach authentication variables
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/sc_functions_2025.R') 

# Connect to SC
sc <- sc_setup(cid,tkn)

# Player Data
playerList <- sc_players(sc)
playerStats <- bind_rows(lapply(playerList$playerID, function(p){
  s <- sc_playerStats(sc, p)
}))

# Average Draft Position
playerADP <- lapply(playerList$playerID, function(p){
  data <- sc_download(sc$auth, sprintf(sc$url$player,p))
  adp <- data$player_stats[[1]]$adp
  adp <- ifelse(is.null(adp),NA,adp)
  return(adp)
})
names(playerADP) <- playerList$playerID
playerList$ADP <- unlist(playerADP)

# Fixture Analysis
aflFixture <- sc_download(sc$auth,sc$url$aflFixture)
aflFixture <- bind_rows(lapply(aflFixture, function(f) unlist(f, use.names=TRUE)))



ladder <- fetch_ladder(season = 2025, source = "squiggle")



x <- fetch_squiggle_data("ladder", year = 2025)

x %>%
  group_by(team) %>%
  summarise(
    rank= mean(mean_rank),
    wins =median(mean_rank)
  ) %>%
  mutate(r = round(wins,0)) %>%
  arrange(wins)


