source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') # import supercoach authentication variables

### BASE DATA ------------------------------------------------------------------

sc <- get_sc(cid,tkn)
playerList <- get_playerList(sc)

adp_raw <- lapply(playerList$id, function(id){
  url <- sprintf(sc$url$player, id)
  get_sc_data(sc$auth, url)
})

adp_data <- tibble(
  id = unlist(lapply(adp_raw, function(x) x$id)),
  feed_id = unlist(lapply(adp_raw, function(x) x$feed_id)),
  player_name = NA,
  first_name = unlist(lapply(adp_raw, function(x) x$first_name)),
  last_name = unlist(lapply(adp_raw, function(x) x$last_name)),
  team = unlist(lapply(adp_raw, function(x) x$team$abbrev)),
  pos = unlist(lapply(adp_raw, function(x) {
    paste0(lapply(x$positions, function(p) p$position), collapse = '/')
  })),
  avg = round(unlist(lapply(adp_raw, function(x) x$previous_average)),0),
  adp = round(unlist(lapply(adp_raw, function(x){
    adp <- x$player_stats[[1]]$adp
    if(is.null(adp)){adp <- NA} 
    adp
  })),1),
) %>%
  filter(substr(last_name,1,3)!='***') %>% # Remove retired players
  mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
  group_by(player_name, team) %>%
  mutate(n = n()) %>%
  mutate(player_name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), player_name)) %>%
  ungroup() %>%
  select(-n) %>%
  arrange(adp)

write.csv(adp_data, paste0('./scripts/021 Draft Prep/2024/adp ',Sys.Date(),'.csv'), na='', row.names=FALSE)

### Draft Doctors ADP ----------------------------------------------------------
dd_raw <- readLines('./scripts/021 Draft Prep/2024/data/DraftDoctorsADP.json')
dd_raw <- fromJSON(dd_raw)

name_match <- adp_data %>%
  select(feed_id, first_name, last_name, team, avg) %>%
  mutate(name = paste0(first_name, ' ', last_name)) %>%
  group_by(name) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(name, feed_id)

dd_data <- tibble(
  #id = unlist(lapply(dd_raw, function(x) x$id)),
  name = unlist(lapply(dd_raw, function(x) x$name)),
  team = unlist(lapply(dd_raw, function(x) x$team)),
  positions = unlist(lapply(dd_raw, function(x) paste0(substr(x$positions,1,3),collapse = "/"))),
  rank1 = unlist(lapply(dd_raw, function(x) x$supercoachRanking$draftDoctorsRank1)),
  rank2 = unlist(lapply(dd_raw, function(x) x$supercoachRanking$draftDoctorsRank2)),
  avg = unlist(lapply(dd_raw, function(x) x$supercoachRanking$average)),
  #computedRank = unlist(lapply(dd_raw, function(x) x$supercoachRanking$computedRank)),
  adp = round(as.numeric(unlist(lapply(dd_raw, function(x) x$supercoachRanking$averageDraftPosition))),1),
  adp_sd = round(as.numeric(unlist(lapply(dd_raw, function(x) x$supercoachRanking$adpStandardDeviation))),3),
  #adp_max = round(as.numeric(unlist(lapply(dd_raw, function(x) x$supercoachRanking$maxAdp))),3)
) %>%
  left_join(name_match, by=c('name')) %>%
  filter(adp >0) %>%
  filter(adp_sd >0) %>%
  arrange(adp) %>%
  rename(adp_dd = adp) %>%
  select(feed_id, adp_dd)

adp_data2 <- adp_data %>%
  left_join(dd_data, by=c('feed_id')) %>%
  mutate(diff = abs(adp - adp_dd)) %>%
  filter(adp <= 200 | adp_dd <= 200)

plot(x=adp_data2$adp,
     y=adp_data2$adp_dd)

adp_data <- adp_data %>%
  left_join(dd_data, by=c('feed_id')) %>%
  mutate(adp = adp_dd) %>%
  select(-adp_dd, -first_name, -last_name) %>%
  arrange(adp)

### Historical Data-- ----------------------------------------------------------

history_data <- read_csv('./scripts/021 Draft Prep/2024/data/player_data_2024.csv')

### PROJECTION ENGINE ----------------------------------------------------------

base <- adp_data %>%
  filter(adp <= 200)

plot(base$adp, 
     base$avg, 
     col = case_when(grepl('FWD',base$pos) ~ 'red',
                     grepl('DEF',base$pos) ~ 'blue',
                     grepl('RUC',base$pos) ~ 'green',
                     TRUE                  ~ 'black'), 
     main="ADP to Average",
     xlab="Average Draft Position", 
     ylab="Previous Average", 
     pch=20)                                    # Plot the data points

legend("bottomleft",   # Change the position as per your preference
       legend = c('FWD','DEF','RUC','MID'),  # Assumes base$pos is categorical
       col = c("red", "blue", "green", "black"),  # Colors corresponding to the categories
       pch = 20,  # The same point shape used in the plot
       title = "Position")  # Title for the legend


proj_model <- function(data, pos){
  
  data1 <- data %>%
    filter(adp <= 200)
  
  data2 <- data1[grepl(pos, data1$pos),]
  
  data3 <- data2 %>%
    filter(avg > 0)
  
  degree <- 2                                     # Define the degree of the polynomial
  model <- lm(data3$avg ~ poly(data3$adp, degree, raw = TRUE))    # Fit the polynomial regression model
  
  residuals <- residuals(model)                   # Get the residuals
  residuals_mean <- mean(residuals)               # Calculate the mean and standard deviation of residuals
  residuals_sd <- sd(residuals)
  threshold <- 1.5 * residuals_sd                   # Set a threshold for outliers 
  outliers <- which(abs(residuals) > threshold)   # Identify outliers based on the threshold
  
  x_clean <- data3$adp[-outliers]                         # Remove outliers from the dataset
  y_clean <- data3$avg[-outliers]
  model_clean <- lm(y_clean ~ poly(x_clean, degree, raw = TRUE))    # Fit model to the cleaned dataset
  predicted_clean <- predict(model_clean)                           # Generate predicted Y values
  
  plot(data2$adp, 
       data2$avg, 
       main=pos,
       xlab="Average Draft Position", 
       ylab="Previous Average", 
       pch=20,
       col='red')   
  
  
  
  points(data2$adp[data2$feed_id %in% data3$feed_id[-outliers]],
         data2$avg[data2$feed_id %in% data3$feed_id[-outliers]], col='black',pch=20)
  
  lines(sort(x_clean), predicted_clean[order(x_clean)], col="red", lwd=2)
  
  #predicted_y <- predict(model_clean, newdata = data.frame(x_clean = new_x))
  
  return(model_clean)
}

pos <- c('MID','DEF','RUC','FWD')

model <- lapply(pos, function(p) proj_model(adp_data, p))
names(model) <- pos

proj <- lapply(model, function(m){
  round(predict(m, newdata = data.frame(x_clean = adp_data$adp)),0)
})
names(proj) <- pos

proj_data <- adp_data %>%
  mutate(draft_pos = NA) %>%
  mutate(draft_proj = NA) %>%
  mutate(draft_tier = NA)

proj_data$draft_pos[grepl('MID', proj_data$pos)] <- 'MID'
proj_data$draft_proj[grepl('MID', proj_data$pos)] <- proj[['MID']][grepl('MID', proj_data$pos)]

proj_data$draft_pos[grepl('RUC', proj_data$pos)] <- 'RUC'
proj_data$draft_proj[grepl('RUC', proj_data$pos)] <- proj[['RUC']][grepl('RUC', proj_data$pos)]

proj_data$draft_pos[grepl('DEF', proj_data$pos)] <- 'DEF'
proj_data$draft_proj[grepl('DEF', proj_data$pos)] <- proj[['DEF']][grepl('DEF', proj_data$pos)]

proj_data$draft_pos[grepl('FWD', proj_data$pos)] <- 'FWD'
proj_data$draft_proj[grepl('FWD', proj_data$pos)] <- proj[['FWD']][grepl('FWD', proj_data$pos)]

proj_data$draft_tier <- proj[['MID']]

proj_final <- proj_data %>%
  mutate(draft_tier = case_when(draft_tier >= 115 ~ 1,
                                draft_tier >= 110 ~ 2,
                                draft_tier >= 105 ~ 3,
                                draft_tier >= 100 ~ 4,
                                draft_tier >=  95 ~ 5,
                                draft_tier >=  85 ~ 6,
                                TRUE              ~ 7)) %>%
  group_by(draft_pos) %>%
  mutate(pos_rank = paste0(substr(draft_pos,1,1),' ',row_number())) %>%
  ungroup() %>%
  select(feed_id, draft_proj, pos_rank, draft_tier)


strat <- lapply(proj, function(x){
  
})

a <- bind_cols(proj) %>%
  mutate(pick = row_number()) %>%
  filter(pick/8 == ceiling(pick/8)) %>%
  filter(pick <= 200) %>%
  pivot_longer(cols=!contains('pick')) %>%
  arrange(desc(pick)) %>%
  group_by(name) %>%
  mutate(diff = value - lag(value)) %>%
  arrange(pick) %>%
  ungroup() %>%
  filter(name != 'RUC') %>%
  group_by(pick) %>%
  mutate(flag = max(diff) == diff) %>%
  filter(flag) %>%
  select(pick, name, value) %>%
  pivot_wider()




### AGE ANALYSIS ---------------------------------------------------------------

# Download fanfooty player details
ff_players <- fread('http://www.fanfooty.com.au/resource/player.php')
colnames(ff_players) <- c('ff_id','feed_id','first_name','last_name','team','status','number','dob','height','weight','state','recruit','career_games','goals')

age <- adp_data %>%
  mutate(feed_id = as.numeric(feed_id)) %>%
  left_join(ff_players[,c('feed_id','dob','career_games')], by='feed_id') %>%
  mutate(feed_id = as.character(feed_id)) %>%
  mutate(career_games = ifelse(is.na(career_games),0,career_games)) %>%
  mutate(age = year(Sys.Date())-year(dob)) %>%
  mutate(age_flag = case_when(career_games == 0 ~ "Y",  # First year
                              age == 30         ~ "O",  # Caution
                              age >  30         ~ "XO")) # Do not draft

age_final <- age %>%
  select(feed_id, age_flag)

### INJURY ANALYSIS ------------------------------------------------------------

inj <- history_data %>%
  filter(!is.na(opp.abbrev)) %>%
  arrange(feed_id, desc(season), desc(round)) %>%
  group_by(feed_id) %>%
  mutate(games_played = cumsum(played)) %>%
  ungroup() %>%
  filter(games_played > 0) %>%
  mutate(dropped = ifelse(lag(feed_id)==feed_id, played == 1 & lag(played)==0, NA)) %>%
  filter(played == 1) %>%
  select(feed_id, player_name, season, round, played, dropped) %>%
  mutate(play = rollapply(played, 50, sum, align='left',partial=TRUE)) %>%
  mutate(drop = rollapply(dropped,50, sum, align='left',partial=TRUE)) %>%
  mutate(consist = round(1-(drop/play),3)) %>%
  ungroup() 

inj2 <- inj %>%
  arrange(feed_id, desc(season), desc(round)) %>%
  group_by(feed_id) %>%
  filter(!is.na(consist)) %>%
  filter(row_number() == 1) %>%
  select(feed_id, player_name, consist) %>%
  mutate(feed_id = as.character(feed_id)) %>%
  left_join(adp_data, by=c('feed_id')) 

plot(x=inj2$consist, y=inj2$avg, pch=20)

x <- inj %>%
  filter(player_name == 'S.Flanders') %>%
  select(player_name, season, round, played, play, drop, consist)

inj_final <- inj2 %>%
  mutate(inj_flag = case_when(consist <= 0.85 ~ "XA", 
                              consist <= 0.9  ~ "A")) %>%  # Do not draft
  select(feed_id, inj_flag)

### NEWS UPDATES ---------------------------------------------------------------

news <- read.csv('./scripts/021 Draft Prep/2024/data/notes 2024-03-02.csv') %>%
  select(ID_CD, ADP, Notes) %>%
  mutate(feed_id = trimws(substr(ID_CD, 5,25))) %>%
  select(feed_id, Notes)


### FIXTURE ANALYSIS ------------------------------------------------------------

library(fitzRoy)

fixture <- get_sc_data(sc$auth, sc$url$aflFixture)

fixture_data <- tibble(
  id = unlist(lapply(fixture, function(x) x$id)),
  season = unlist(lapply(fixture, function(x) x$season)),
  round  = unlist(lapply(fixture, function(x) x$round)),
  team1  = unlist(lapply(fixture, function(x) x$team1$abbrev)),
  team2  = unlist(lapply(fixture, function(x) x$team2$abbrev)),
) %>%
  pivot_longer(cols=c(team1,team2))

byes <- fixture_data %>%
  select(round, value) %>%
  pivot_wider(names_from=value) %>%
  pivot_longer(!contains('round')) %>%
  filter(is.na(value)) %>%
  select(-value) %>%
  group_by(round) %>%
  mutate(n= n()) %>% 
  filter(round > 11 | round == 0) %>%
  ungroup() %>%
  mutate(n = ifelse(round == 0,n,7-n)) %>%
  group_by(name) %>%
  summarise(tot = sum(n)) %>%
  ungroup() %>%
  mutate(byes = match(tot, sort(unique(tot)))-1) %>%
  select(-tot)
  
#ladder <- fetch_ladder(season = 2023, source = "squiggle")

squiggle <- tibble(
  team = c('BRL','MEL','GWS','COL','ADE','PTA','GEE','SYD','CAR','WBD','STK','FRE','GCS','RIC','HAW','ESS','NTH','WCE')
) %>%
  mutate(rank = row_number()) %>%
  mutate(section = 5-as.numeric(cut(rank, breaks=4, labels=c(1:4)))) 
  
finals <- fixture_data %>%
  filter(round>=max(round)-2) %>%
  left_join(squiggle, by=c('value'='team')) %>%
  select(id, value, section)

finals2 <- finals %>%
  left_join(finals, by=c('id'), relationship='many-to-many') %>%
  filter(value.x != value.y) %>%
  rowwise() %>%
  mutate(diff = max(section.x - section.y-1,0)) %>%
  ungroup() %>%
  group_by(value.x) %>%
  summarise(finals = sum(diff)) %>%
  arrange(desc(finals))

fixture_final <- byes %>%
  left_join(finals2, by=c('name'='value.x')) %>%
  mutate(total = byes + finals) %>%
  arrange(desc(total)) %>%
  mutate(fixture_flag = paste0('F',total)) %>%
  select(name, fixture_flag)
  



### TARGETS AND EXCLUSIONS------------------------------------------------------


# Dawson
# Green
# Serong
# Houston
# Grundy
# Short
# Warner
# ROB


# Rankine
# Rayner

# James Jordan
# James Harmes
# Jack Billings
# Harley Reid
# C Lazzaro
# Wardlaw
# F Macrae
# A Sexton

### PRE SEASON -----------------------------------------------------------------

ff_games <- fread('http://www.fanfooty.com.au/resource/draw.php')
colnames(ff_games) <- c('game_id','year','competition','round','gametime_AET','day','home_team','away_team','ground','timeslot','TV_coverage','home_supergoals','home_goals','home_behinds','home_points','away_supergoals','away_goals','away_behinds','away_points','match_status')

ff_games1 <- ff_games %>%
  filter( year == year(Sys.Date())) %>%
  filter(trimws(match_status) == 'Full Time')

preseason <- tibble()
for( i in 1:nrow(ff_games1)){
  
  game_id <- ff_games1$game_id[i]
  
  url <- paste0("https://www.fanfooty.com.au/live/", game_id, ".txt")
  game_data <- strsplit(readLines(url),",", useBytes = TRUE)
  
  game_data <- as.data.frame(t(as.data.frame(game_data[5:length(game_data)])))
  row.names(game_data) <- NULL
  
  colnames(game_data) <- c(
    'feed_id',
    'first_name',
    'last_name',
    'team_ff',
    'disposals',
    'dreamteam',
    'supercoach',
    8,
    9,
    10,
    'kicks',
    'handballs',
    'marks',
    'tackles',
    'hitouts',
    'frees_for',
    'frees_against',
    'goals',
    'behinds',
    'gametime',
    'icon_1',
    'desc_1',
    'icon_2',
    'desc_2',
    25,
    26,
    27,
    28,
    'position',
    'jersey',
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    'contested_possessions',
    'clearances',
    'clangers',
    'disposal_effeciency',
    'time_on_ground',
    'metres_gained',
    'bench_flag'
  )
  
  preseason <- bind_rows(preseason, game_data)
  
}

preseason_final <- preseason %>%
  mutate(supercoach = as.numeric(supercoach)) %>%
  arrange(desc(supercoach)) %>%
  select(feed_id, supercoach) %>%
  left_join(adp_data, by=c('feed_id')) %>%
  filter(supercoach >= 90) %>%
  select(feed_id, supercoach)
  


### CREATE NOTES ---------------------------------------------------------------


notes <- adp_data %>%
  left_join(proj_final, by=c('feed_id')) %>%
  left_join(age_final, by=c('feed_id')) %>%
  left_join(inj_final, by=c('feed_id')) %>%
  left_join(fixture_final, by=c('team'='name')) %>%
  left_join(news, by=c('feed_id')) %>%
  left_join(preseason_final, by=c('feed_id')) %>%
  filter(age_flag != 'Y' | is.na(age_flag)) %>%
  mutate(adp = ifelse(is.na(supercoach) | adp < 161, adp, 161+(200-supercoach)/1000)) %>%
  arrange(adp) %>%
  mutate(rank = row_number()) %>%
  mutate(round = ceiling(rank/8)) %>%
  mutate(DEF = ifelse(grepl('DEF',pos), player_name, NA)) %>%
  mutate(MID = ifelse(grepl('MID',pos), player_name, NA)) %>%
  mutate(RUC = ifelse(grepl('RUC',pos), player_name, NA)) %>%
  mutate(FWD = ifelse(grepl('FWD',pos), player_name, NA)) %>%
  mutate(flag = paste0(fixture_flag, 
                       ifelse(is.na(age_flag),'',age_flag), 
                       ifelse(is.na(inj_flag),'',inj_flag), 
                       ifelse(is.na(supercoach),'',paste0('P',supercoach)))) %>%
  select(round, rank, draft_tier, pos_rank,
         DEF,MID,RUC,FWD,
         team, avg, draft_proj,
         flag, Notes) %>%
  rename(Rd = round,
         P = rank,
         `T` = draft_tier,
         R = pos_rank,
         proj = draft_proj) %>%
  filter(Rd <= 32)
  
write.csv(notes, './scripts/021 Draft Prep/2024/notes.csv', na='', row.names=FALSE)




