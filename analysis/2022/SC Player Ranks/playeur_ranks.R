library(zoo)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

savePath <- './analysis/2022/SC Player Ranks/'

# Historical Data

path <- './data/2020/raw'
files <- list.files(path)
files <- files[grepl('SC_PLAYER', files)]

player_data <- tibble()
for (file in files){
  filepath <- paste0(path,'/',file)
  sc_players <- readRDS(filepath)
  pData <- get_sc_player_data(sc_players)
  player_data <- bind_rows(player_data,pData)
}
player_data$season <- 2020
players <- player_data


path <- './data/2021/raw'
files <- list.files(path)
files <- files[grepl('sc_players', files)]

player_data <- tibble()
for (file in files){
  filepath <- paste0(path,'/',file)
  sc_players <- readRDS(filepath)
  pData <- get_sc_player_data(sc_players)
  player_data <- bind_rows(player_data,pData)
}
player_data$season <- 2021
players<-bind_rows(players,player_data)


# Current Season

sc <- get_sc(cid,tkn)
player_data <- get_player_data(sc)
player_data$season <- 2022
players<-bind_rows(players,player_data) %>%
  inner_join(player_data['feed_id'], by=c('feed_id')) %>%
  distinct()

#

ranks <- players %>%
  mutate(pts = points) %>%
  select(feed_id, season, round, pts) %>%
  arrange(feed_id, season, round) %>%
  group_by(feed_id) %>%
  filter(!is.na(pts)) %>%
  mutate(median = rollapply(pts, 11, median, align='right', partial=T)) %>%
  right_join(players, by=c('feed_id','season','round')) %>%
  arrange(feed_id, season, round) %>%
  fill(median) %>%
  mutate(na_flg = is.na(pts)) %>%
  mutate(na_streak=sequence(rle(na_flg)$lengths)) %>%
  mutate(pts_adj = ifelse(is.na(pts),ifelse(median>75,75,median)+0.5-(na_streak*0.5),pts)) %>%
  mutate(median_adj = rollapply(pts_adj, 11, median, align='right', partial=T)) %>%
  mutate(median_fnl = ifelse(is.na(pts),median_adj,median)) %>%
  arrange(feed_id, desc(season), desc(round)) %>%
  mutate(n = row_number()) %>%
  #filter(feed_id == 990704)
  filter(n == 1) %>%
  ungroup() %>%
  arrange(desc(median_fnl))

  # Standardise averages
pos_list <- tibble(
  p = c('DEF','MID','RUC','FWD'),
  n = c(5+1,7+2,2+0,5+1)
) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(weight = round(n/sum_n,3))
  
scr_data <- tibble()
overall_smy <- tibble()
for(i in 1:nrow(pos_list)) {
  
  p <- pos_list$p[i]
  n <- pos_list$n[i]
  
  pos_data <- ranks[grepl(p,ranks$pos_1)|grepl(p,ranks$pos_2),]

  pos_smy <- pos_data  %>%
    select(feed_id, median_fnl) %>%
    arrange(desc(median_fnl)) %>%
    top_n(n*8, median_fnl)
  
  mean<- mean(pos_smy$median_fnl)
  sd<- sd(pos_smy$median_fnl)
  
  scr <- pos_data %>%
    mutate(pos_scr = round((median_fnl - mean)/sd,3)) %>%
    select(feed_id, pos_scr) %>%
    mutate(scr_pos = p)
  
  scr_data <- bind_rows(scr_data, scr)
  overall_smy <- bind_rows(overall_smy, pos_smy)
}  

scr_data <- scr_data %>%
  arrange(desc(pos_scr)) %>%
  distinct(feed_id, .keep_all=T)
  
mean <- mean(overall_smy$median_fnl)
sd <- sd(overall_smy$median_fnl)

ranks2 <- ranks %>%
  mutate(scr = round((median_fnl - mean)/sd,3)) %>%
  left_join(scr_data, by='feed_id') %>%
  left_join(pos_list[,c('p','weight')], by=c('scr_pos'='p')) %>%
  mutate(scr_wgt = pos_scr*weight + scr*(1-weight)) %>%
  arrange(desc(scr_wgt)) %>%
  mutate(rank = row_number()) %>%
  mutate(rating = case_when(scr_wgt >= min(ifelse(median_fnl>=110,scr_wgt,NA), na.rm=T)  ~ 'ELITE',
                       scr_wgt >= min(ifelse(median_fnl>=100,scr_wgt,NA), na.rm=T)  ~ 'PREMO',
                       rank <= 152                                                  ~ 'SC RELEVANT',
                       rank <= 200                                                  ~ 'WAIVER QUALITY',
                       TRUE                                                         ~ 'SC IRRELEVANT'))


smy <- ranks2 %>%
  group_by(x) %>%
  summarise(n = n())
           
           
ranks3 <- ranks2 %>%
  filter(rank <= 300)

write_csv(ranks3, './analysis/2022/SC Player Ranks/pmacs_player_ratings.csv', na='')


