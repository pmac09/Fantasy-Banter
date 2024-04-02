library(tidyverse)
library(zoo)
library(highcharter)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') # import supercoach authentication variables

sc <- get_sc(cid, tkn)

playerList <- get_sc_data(sc$auth, paste0(sc$url$players,'0'))

playerStats <- lapply(playerList, function(p) {
  get_sc_data(sc$auth, paste0(sc$url$statsPack,p$id))$playerStats
})
playerStats1 <- lapply(playerStats, function(ps){
  data1 <- lapply(ps, unlist)
  data2 <- bind_rows(data1)
})
playerStats2 <- bind_rows(playerStats1) %>%
  mutate_if(numeric_col,as.numeric)
  

playerData <- tibble(
  id = unlist(lapply(playerList, function(x) x$id)),
#  feed_id = unlist(lapply(playerList, function(x) x$feed_id)),
  first_name = unlist(lapply(playerList, function(x) x$first_name)),
  last_name = unlist(lapply(playerList, function(x) x$last_name)),
  team = unlist(lapply(playerList, function(x) x$team$abbrev)),
  pos1 = unlist(lapply(playerList, function(x) x$positions[[1]]$position)),
  pos2 = unlist(lapply(playerList, function(x) ifelse(length(x$positions)>1,x$positions[[2]]$position,NA))),
) %>%
  left_join(playerStats2, by=c('id'='player_id'))


path <-paste0('./scripts/021 Draft Prep/2024/player_data.csv')

write.csv(playerData,path,na='',row.names = F)


playerData <- read_csv(path)






getwd()

x$player_stats[[1]]$price

https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&round=0

https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/completeStatspack?player_id=20


path <- './data/2023/raw/'
files <- list.files(path)
files <- files[grep('^completeStatspack',files)]

player_data <- tibble()
for (i in 1:length(files)){
  
  filepath <- paste0(path, 'completeStatspack?player_id=',i,'.rds')
  data <- readRDS(filepath)$playerStats

  if(is.null(data)) next
  
  data1 <- lapply(data, unlist)
  data2 <- bind_rows(data1)
  
  player_data <- bind_rows(player_data, data2)
  
}

numeric_col <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}

player_data <- player_data %>%
  mutate_if(numeric_col,as.numeric) %>%
  select(player_id, 
         team.abbrev,
         season,
         round,
         played,
         points,
         opp.abbrev)

path <- './data/2023/raw/'
files <- list.files(path)

filepath <- paste0(path, 'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=24.rds')
data <- readRDS(filepath)

player_list <- tibble(
  id = unlist(lapply(data, function(x) x$id)),
  feed_id = unlist(lapply(data, function(x) x$feed_id)),
  first_name = unlist(lapply(data, function(x) x$first_name)),
  last_name = unlist(lapply(data, function(x) x$last_name)),
  team = unlist(lapply(data, function(x) x$team$abbrev)),
  pos1 = unlist(lapply(data, function(x) x$positions[[1]]$position)),
  pos2 = unlist(lapply(data, function(x) ifelse(length(x$positions)>1,x$positions[[2]]$position,NA)))
)

data <- player_list %>%
  inner_join(player_data, by=c('id'='player_id')) %>%
  mutate(season_round = as.numeric(paste0(season-2000,'.', formatC(round, width = 2, format = "d", flag = "0")))) %>%
  arrange(feed_id, season_round) %>%
  group_by(feed_id) %>%
  mutate(games_played = cumsum(played)) %>%
  ungroup() %>%
  filter(games_played > 0)

player_median <- data %>%
  filter(played == 1) %>%
  arrange(feed_id, season_round) %>%
  group_by(feed_id) %>%
  mutate(median = rollapply(points, 15, median, align='right', partial=TRUE)) 

data1 <- data %>%
  left_join(player_median[,c('feed_id','season_round','median')]) %>%
  arrange(desc(feed_id), season_round) %>%
  group_by(feed_id) %>%
  fill(median, .direction = "down") %>%
  mutate(drop_flag = ifelse(played==0 & lag(played==1) & !is.na(opp.abbrev),1,0)) %>%
  mutate(play = rollapply(played,50, sum, align='right',partial=TRUE)) %>%
  mutate(drop = rollapply(drop_flag,50, sum, align='right',partial=TRUE)) %>%
  mutate(consist = round(1-(drop/play),3)) %>%
  ungroup()


x <- data1 %>%
  filter(season_round == max(season_round))


z <- data1 %>%
  filter(first_name == 'Clayton') %>%
  arrange(season,round) %>%
  mutate(row_number = row_number()) 
 

# highchart() %>%
#   hc_chart(type = 'column') %>%
#   hc_title(text = 'Performance by Selection Criteria') %>%
#   hc_subtitle(text= 'magoos of the Week') %>%
#   hc_xAxis(categories=motw_summary$round) %>%
#   hc_add_series_list(motw_type_hc) 


highchart() %>% 
  hc_chart(type='column') %>%
  hc_xAxis(categories=z$season_round) %>%
  hc_add_series(z$points) %>%
  hc_add_series(z$median, type='spline')
  







