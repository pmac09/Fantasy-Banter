source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

# Create data placeholders
player_data <- tibble()
team_data <- tibble()

# Gather data for selected rounds
for(i in 1:23){
  log(paste0('Round ', i))
  
  # Gather player data
  sc_players <- readRDS(paste0('./data/2021/raw/sc_players_',i,'.rds'))
  p_data <- get_sc_player_data(sc_players)
  player_data <- bind_rows(player_data, p_data)
  
  # Gather league data
  sc_league <- readRDS(paste0('./data/2021/raw/sc_league_',i,'.rds'))
  
  # Gather team data
  t_data <- get_sc_team_data(sc_league)
  team_data <- bind_rows(team_data, t_data)
}

# Join teams to player data
player_data <- player_data %>%
  left_join(team_data, by=c('round', 'player_id'))

write.csv(player_data, './data/2021/clean/players_2021.csv', na='', row.names=F)


league_2021 <- tibble()
for(i in 1:23){
  sc_league <- readRDS(paste0('./data/2021/raw/sc_league_',i,'.rds'))
  
  fixture <- get_sc_fixture_data(sc_league)
  ladder <- get_sc_ladder_data(sc_league) %>%
    dplyr::select(-teamname, -coach, -round)
  
  data <- fixture %>%
    left_join(ladder, by=c('team_id'))
  
  league_2021 <- bind_rows(league_2021, data)
}

league_2021[c(169:174),c(16:22)] <- NA
league_2021$coach[c(169:174)]
league_2021$position[c(169:174)] <- c(3,1,2,4,2,1)

write.csv(league_2021, './data/2021/clean/league_2021.csv', na='', row.names=F)