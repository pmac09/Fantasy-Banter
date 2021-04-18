library(tidyverse)
library(DT)

# scTeams <- rbindlist(supercoach, fill = TRUE) %>%
#   mutate(name = paste0(fi,'.',ln)) %>%
#   filter(status == 'playing') %>%
#   filter(owner_id %in% c('397', '344')) %>%
#   select(owner, name, avg, ppts1, owner_id) %>%
#   rename(proj = ppts1) %>%
#   arrange(owner)

ffscores <- tibble()
for(i in 7097:7098){
  
  gameNo <- i
  
  url <- paste0("http://live.fanfooty.com.au/chat/", gameNo, ".txt")
  data <- tryCatch({strsplit(readLines(url),",")})
  
  data2 <- data.frame(matrix(unlist(data[5:length(data)]), nrow=length(data)-4, byrow=T),stringsAsFactors=FALSE) %>%
    rename(PLAYER_ID = X1) %>%
    rename(SUPERCOACH = X7) %>%
    mutate(NAME = paste0(substr(X2,1,1),'.',X3)) %>%
    mutate(GAME_ID = i) %>%
    select(NAME, SUPERCOACH, GAME_ID)
  
  ffscores <- as_tibble(rbind(ffscores, data2))
}



gameScores <- left_join(scTeams, ffscores, by=c('name'='NAME')) %>%
  filter(!(owner == 'TryingAnewStratton' & (name %in% c('I.Smith','B.Hardwick')))) %>%
  filter(!(owner == 'Man of Steele'      & (name %in% c('A.Cerra','R.Burton')))) %>%
  mutate(SUPERCOACH = as.numeric(SUPERCOACH)) %>%
  arrange(GAME_ID, desc(SUPERCOACH)) %>%
  add_row(owner='TryingAnewStratton', name= 'TryingAnewStratton', avg=1494,  proj=1494,  SUPERCOACH = 1494, .before=1) %>%
  add_row(owner='Man of Steele'     , name= 'Man of Steele',      avg=1438,  proj=1438,  SUPERCOACH = 1438, .before=1) 

gameScores$avg <- as.numeric(gameScores$avg)
gameScores$proj <- as.numeric(gameScores$proj)

totalScores <- gameScores %>%
  group_by(owner) %>%
  summarise(
    AVG_TOTAL = sum(avg),
    PROJ_TOTAL = sum(proj),
    LIVE_TOTAL = sum(SUPERCOACH, na.rm=T)
  )

names(gameScores)[3:5] <- names(totalScores)[2:4]

final <- bind_rows(gameScores, totalScores)


game1T1 <- final %>%
  filter(owner== 'TryingAnewStratton') %>%
  select(name, AVG_TOTAL, PROJ_TOTAL, LIVE_TOTAL)
  
game1T2 <- final %>%
  filter(owner== 'Man of Steele') %>%
  select(name, AVG_TOTAL, PROJ_TOTAL, LIVE_TOTAL)

game1 <- cbind(game1T1, game1T2)



game2T1 <- final %>%
  filter(owner== 'Salt&VinegarCripps') %>%
  select(name, AVG_TOTAL, PROJ_TOTAL, LIVE_TOTAL)

game2T2 <- final %>%
  filter(owner== 'Man of Steele') %>%
  select(name, AVG_TOTAL, PROJ_TOTAL, LIVE_TOTAL)

game2 <- cbind(game2T1, game2T2)


game1
game2








