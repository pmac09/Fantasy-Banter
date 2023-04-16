library(tidyverse)
library(plyr)
library(zoo)
library(rjson)


path <- paste0('./scripts/021 Draft Prep/2023/')

# SC ADP
adp <- fromJSON(file=paste0(path,'SC_ADP_230308.json'))
adp1 <- bind_rows(lapply(adp, function(x) tibble(feed_id = x$feed_id,adp = x$player_stats[[1]]$adp))) %>%
  mutate(feed_id = as.numeric(feed_id))

# SC Player List
plist <- read.csv(paste0(path,'sc_player_list.csv')) %>%
  mutate(player_id = as.numeric(player_id))

ranking <- plist %>%
  left_join(adp1, by=c('player_id'='feed_id')) %>%
  mutate(adp_round = ceiling(adp/8))

write.csv(ranking, paste0(path,'sc_draft_ranks.csv'), na='', row.names = F)





data <- read.csv(paste0(path,'sc_player_history.csv'))


# Long Term Performance
med50 <- data %>%
  filter(!is.na(points)) %>%
  arrange(feed_id,desc(season),desc(round)) %>%
  group_by(feed_id) %>%
  mutate(recent = row_number()) %>%
  arrange(feed_id,season,round) %>%
  mutate(n= 1) %>%
  mutate(n50 = rollapply(n, 50, sum, partial=T, align = 'right')) %>%
  mutate(median50 = rollapply(points, 50, median, partial=T, align = 'right')) %>%
  mutate(lowerQ50 = rollapply(points, 50, FUN = "quantile", p = .25, partial=T, align = 'right')) %>%
  filter(recent == 1) %>%
  mutate(long_form = cut(median50, c(-Inf,seq(70,130,10),Inf), c('<=70', paste0(">",seq(70,130,10))))) %>%
  arrange(desc(long_form), desc(lowerQ50)) %>%
  select(feed_id, n50, median50, lowerQ50, long_form)



short_form <- data %>%
  filter(!is.na(points)) %>%
  filter(season==max(season)) %>%
  arrange(feed_id,desc(season),desc(round)) %>%
  group_by(feed_id) %>%
  mutate(recent = row_number()) %>%
  arrange(feed_id,season,round) %>%
  mutate(n= 1) %>%
  mutate(n10 = rollapply(n, 10, sum, fill=NA, align = 'right')) %>%
  mutate(median10 = rollapply(points, 10, median, fill=NA, align = 'right')) %>%
  summarise(
    median10 = median(median10, na.rm=T)
  ) %>%
  arrange(desc(median10)) %>%
  mutate(short_form = cut(median10, c(-Inf,seq(70,130,10),Inf), c('<=70', paste0(">",seq(70,130,10)))))
  

ceiling <- data %>%
  filter(!is.na(points)) %>%
  filter(season>=max(season)-1) %>%
  arrange(feed_id,desc(season),desc(round)) %>%
  group_by(feed_id) %>%
  mutate(recent = row_number()) %>%
  arrange(feed_id,season,round) %>%
  mutate(n= 1) %>%
  mutate(n10 = rollapply(n, 10, sum, fill=NA, align = 'right')) %>%
  mutate(median10 = rollapply(points, 10, median, fill=NA, align = 'right')) %>%
  summarise(
    best10 = max(median10, na.rm=T)
  ) %>%
  arrange(desc(best10)) %>%
  mutate(ceiling = cut(best10, c(-Inf,seq(70,130,10),Inf), c('<=70', paste0(">",seq(70,130,10)))))
  

draftOrder <- plist %>%
  select(player_id, last_name,team, pos, average) %>%
  left_join(med50, by=c('player_id'='feed_id')) %>% 
  left_join(short_form, by=c('player_id'='feed_id')) %>% 
  left_join(ceiling, by=c('player_id'='feed_id')) %>% 
  arrange(desc(short_form), desc(long_form), desc(ceiling), desc(lowerQ50)) %>%
  relocate(long_form, .before=average) %>%
  relocate(short_form, .before=average) %>%
  relocate(ceiling, .before=average) 
  


  


