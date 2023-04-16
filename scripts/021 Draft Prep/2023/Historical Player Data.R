# Import common functions
source('./functions/secrets.R')
source('./functions/supercoach_functions.R')

# Read in player data from past seasons
years <- list(`2020` = 'SC_PLAYER_DATA',
              `2021` = 'sc_players',
              `2022` = '^players')

player_data <- tibble()
for (i in 1:length(years)){
  
  year <- names(years)[i]
  year_str <- years[[i]]
  
  path <- paste0('./data/',year,'/raw/')
  files <- list.files(path)
  files <- files[grep(year_str, files)]
  
  for(f in files){
    sc_players <- readRDS(paste0(path,f))
    p_data <- get_sc_player_data(sc_players) %>%
      mutate(season = year)
    player_data <- bind_rows(player_data, p_data)
  }
  
}

write.csv(player_data, 'scripts/021 Draft Prep/2023/sc_player_history.csv', na='', row.names = F)

