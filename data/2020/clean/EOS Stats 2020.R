source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

# Create data placeholders
player_data <- tibble()
team_data <- tibble()

# Gather data for selected rounds
for(i in 1:18){
  log(paste0('Round ', i))
  
  # Gather player data
  sc_players <- readRDS(paste0('./data/2020/raw/2020_',formatC(i, width = 2, format = "d", flag = "0"),'_SC_PLAYER_DATA.RDS'))
  p_data <- get_sc_player_data(sc_players)
  player_data <- bind_rows(player_data, p_data)
  
  # Gather league data
  sc_league <- readRDS(paste0('./data/2020/raw/2020_',formatC(i, width = 2, format = "d", flag = "0"),'_SC_LEAGUE_DATA.RDS'))
  
  # Gather team data
  t_data <- get_sc_team_data(sc_league)
  team_data <- bind_rows(team_data, t_data)
}

# Join teams to player data
player_data <- player_data %>%
  left_join(team_data, by=c('round', 'player_id'))

write.csv(player_data, './data/2020/clean/players_2020.csv', na='', row.names=F)



# Create data placeholders
league_data <- tibble()

# Gather data for selected rounds
for(i in c(1:14,16:17)){
  log(paste0('Round ', i))
  
  # Gather fixture data
  sc_league <- readRDS(paste0('./data/2020/raw/2020_',formatC(i, width = 2, format = "d", flag = "0"),'_SC_LEAGUE_DATA.RDS'))
  
  # Transform
  fData <- get_sc_fixture_data(sc_league)
  
  lData <- get_sc_ladder_data(sc_league) %>%
    dplyr::select(-teamname, -coach, -round)
  
  data <- fData %>%
    left_join(lData, by=c('team_id'))
  
  # bind
  league_data <- bind_rows(league_data, data)

}

write.csv(league_data, './data/2020/clean/league_2020 - req adjustment.csv', na='', row.names=F)

# need to manually adjust
