# import supercoach authentication variables
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') 

# Get base player list

sc <- get_sc(cid,tkn)
playerList <- get_playerList(sc)

season <- sc$var$season

path <- paste0('./scripts/021 Draft Prep/', season, '/data')
file <- paste0(path,'/player_list_', season, '.csv')
write.csv(playerList, file, na='', row.names = F)

# Get historical data

playerStats <- lapply(playerList$id, function(p) {
  get_sc_data(sc$auth, paste0(sc$url$statsPack,p))$playerStats
})
playerStats1 <- lapply(playerStats, function(ps){
  data1 <- lapply(ps, unlist)
  data2 <- bind_rows(data1)
})
playerStats2 <- bind_rows(playerStats1) %>%
  mutate_if(numeric_col,as.numeric)

playerData <- playerList %>%
  left_join(playerStats2, by=c('id'='player_id'))

file <- paste0(path,'/player_data_', season, '.csv')
write.csv(playerData, file, na='', row.names = F)

