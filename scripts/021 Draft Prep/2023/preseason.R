

ff <- get_ff_fixture_data(vSeason=2023)

ff2 <- ff %>%
  filter(match_status!='')


ff3 <- tibble()
for( i in 1:nrow(ff2)){
  
  ff4 <- get_ff_game_data(ff2$game_id[i])
  
  ff3 <- bind_rows(ff3, ff4)
  
}


write.csv(ff3,'preseason.csv')
