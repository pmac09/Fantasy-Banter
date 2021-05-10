source('./functions/supercoach_functions')

draft_data <- read.csv('./data/2021/ASL_Draft_2021_data.csv')

player_data <- get_player_data(cid, tkn)
draft_data <- read.csv('./references/data/2021/ASL_Draft_2021_data.csv')

median_data <- player_data %>%
  group_by(player_id) %>%
  summarise(median_score = median(points, na.rm=T)) %>%
  arrange(desc(median_score))

median_data$median_score[is.na(median_data$median_score)] <- 0

heat_map_data <- player_data %>%
  filter(round == max(round)) %>%
  left_join(median_data, by=c('player_id')) %>%
  left_join(draft_data, by=c('feed_id'='player_id'))

# Create positional flags
positions <- c('DEF','MID','RUC','FWD')
pos_count <- c((8*5), (8*7), (8*1), (8*5))

for (i in 1:4){
  
  pos <- positions[i]
  
  pos_data <- heat_map_data %>%
    filter(pos_1 == pos | pos_2 == pos) %>%
    arrange(desc(median_score))
  
  mean <- mean(pos_data$median_score[1:pos_count[i]])
  stdev <- sd(pos_data$median_score[1:pos_count[i]])
  
  pos_data <- pos_data %>%
    mutate(norm_score = round((median_score-mean)/(stdev),3))
  
  pos_data <- pos_data[,c('player_id','norm_score')]
  colnames(pos_data)[2] <- pos
  
  heat_map_data <-left_join(heat_map_data, pos_data, by=c('player_id'))
}

heat_map_data <- heat_map_data %>%
  rowwise() %>%
  mutate(score = round(max(DEF,MID,RUC,FWD, na.rm=T),4)) %>%
  arrange(pick)

write.csv(heat_map_data, './data/2021/draft_heat_map.csv', na='', row.names=F)
