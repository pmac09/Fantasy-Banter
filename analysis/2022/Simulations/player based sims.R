library(zoo)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

savePath <- './analysis/2022/Simulations/'


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

#get leaguye
league_data <- get_league_data(sc)


#START
# 
team_players <- players %>%
  filter(season==2022) %>%
  filter(round ==max(round)) %>%
  filter(!is.na(coach)) %>%
  arrange(coach, position, desc(avg)) %>%
  group_by(coach,position) %>%
  mutate(n = row_number()) %>%
  filter( (position=='DEF' & n <= 5) |
          (position=='MID' & n <= 7) |
          (position=='RUC' & n <= 2) |
          (position=='FWD' & n <= 5) ) 

# base player data
base_data <- players %>%
  inner_join(team_players[,c('feed_id')], by='feed_id') %>%
  mutate(pts = points) %>%
  select(feed_id, season, round, pts) %>%
  arrange(feed_id, desc(season), desc(round)) %>%
  group_by(feed_id) %>%
  filter(!is.na(pts)) %>%
  mutate(n = row_number()) %>%
  filter(n <= 11) %>% 
  summarise(n = n(),
            min = fivenum(pts)[1],
            Q1 = fivenum(pts)[2],
            median = fivenum(pts)[3],
            Q3 = fivenum(pts)[4],
            max = fivenum(pts)[5]) 

# 
rounds_rem <- league_data %>%
  filter(is.na(team_score)) %>%
  select(round) %>%
  distinct()

#prep data

proj_result <- tibble()

for(i in 1:10000){
  print(i)
  
  proj <- base_data %>%
    full_join(rounds_rem, by = character()) %>%
    rowwise() %>%
    mutate(proj = round(runif(1,Q1,Q3),0)) %>%
    inner_join(team_players[,c('feed_id','coach')], by='feed_id') %>%
    group_by(coach, round) %>%
    summarise(
      proj = sum(proj),
      .groups='drop'
    )
  
  proj_league <- league_data %>%
    left_join(proj, by=c('coach','round')) %>%
    mutate(team_score = ifelse(!is.na(proj),proj,team_score)) %>%
    select(-proj) %>%
    left_join(proj, by=c('opponent_coach'='coach','round')) %>%
    mutate(opponent_score = ifelse(!is.na(proj),proj,opponent_score)) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential>0,1,0)) %>%
    mutate(draw = ifelse(differential==0,1,0)) %>%
    mutate(loss = ifelse(differential<0,1,0)) %>%
    group_by(coach) %>%
    summarise(
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss),
      total_points = sum(team_score),
      .groups='drop'
    ) %>%
    mutate(avg_points = round(total_points/21,0)) %>%
    arrange(desc(wins),desc(total_points)) %>%
    mutate(simulation = i)
  
  proj_result <- bind_rows(proj_result, proj_league)
  
}


proj_result <- proj_result %>%
  group_by(simulation) %>%
  mutate(pos = row_number())



summary <- proj_result %>%
  mutate(TOP4 = ifelse(pos <= 4, 1,0)) %>%
  group_by(coach) %>%
  summarise(
    RANK.MEAN = mean(pos),
    WIN.MAX = max(wins),
    WIN.MIN = min(wins),
    FINALS = sum(TOP4),
    sim = n_distinct(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(PCNT = round(FINALS/sim*100))

