### SETUP --------------------------------------------------------------------
options(stringsAsFactors = FALSE)

suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(fireData))
suppressMessages(library(httr))


### SECRET VARIABLES -----------------------------------------------------------

secret_path <- './www/'
source(paste0(secret_path, 'secrets.R')) # import authentication variables


### GENERAL FUNCTIONS ----------------------------------------------------------

print_log <- function(..., verbose=TRUE){
  if(exists('printLog')) verbose <- printLog
  
  if(verbose){
    time <- format(with_tz(as_datetime(Sys.time()), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S")
    msg <- paste0(time,': ', ...)
    message(msg)
  }
}


### FIREBASE FUNCTIONS ---------------------------------------------------------

fb_download <- function(projectURL, path){
  msg <- paste0('fb_download: ', path)
  print_log(msg)
  
  data <- suppressWarnings(download(projectURL, path))
  return(data)
} # Download data from Firebase location
fb_save <- function(projectURL, path = NULL, data){
  msg <- paste0('fb_save: ', path)
  print_log(msg)
  
  if(is.null(path)) return(NULL)
  put(data, projectURL, path)
} # Save data to Firebase location


### SUPERCOACH FUNCTIONS -------------------------------------------------------

get_sc_auth <- function(cid, tkn){
  msg <- paste0('get_sc_auth')
  print_log(msg)
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/access_token')
  
  # POST request to get an access token
  auth <- content(POST(
    url = url,
    body = list(
      grant_type = 'social',
      client_id = cid,
      client_secret = '',
      service = 'auth0',
      token = tkn
    ),
    encode = 'json'
  ))
  
  sc_auth <- add_headers(
    Authorization = paste0('Bearer ', auth$access_token)
  )
  
  return(sc_auth)
}
get_sc_data <- function(sc_auth, url){
  msg <- paste0('get_sc_data: ',str_extract(url, '(?<=/afl).+'))
  print_log(msg)
  
  sc_data <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  if(!is.null(sc_data$message)){
    msg <- paste0('get_sc_data: ERROR: ',sc_data$message)
    print_log(msg)
    
    sc_data <- NULL
  }
  
  return(sc_data)
}

get_sc <- function(cid, tkn){
  print_log('get_sc')
  
  # Output Variable
  sc <- list()
  
  # Get Authentication
  sc$auth  <- get_sc_auth(cid, tkn)
  
  # URL components
  base <- 'https://supercoach.heraldsun.com.au/'
  year <- year(Sys.Date())
  draft <- '/api/afl/draft/v1/'
  classic <- '/api/afl/classic/v1/'
  
  # Generate API URLs
  sc$url$settings <- paste0(base,year,draft,'settings')
  sc$url$me <- paste0(base,year,classic,'me')
  
  # Call API
  sc$api$settings <- get_sc_data(sc$auth, sc$url$settings)
  sc$api$me <- get_sc_data(sc$auth, sc$url$me)
  
  # Save common variables
  sc$var$user_id <- sc$api$me$id
  
  # Generate API URLs
  sc$url$user <- paste0(base,year,draft,'users/',sc$var$user_id,'/stats')
  
  # Call API
  sc$api$user <- get_sc_data(sc$auth, sc$url$user)
  
  # Save common variables
  sc$var$season <- sc$api$settings$content$season
  sc$var$league_id <- sc$api$user$classic$leagues[[1]]$id
  sc$var$current_round <- sc$api$settings$competition$current_round
  
  # Generate API URLs
  sc$url$players      <- paste0(base,year,draft,'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=')
  sc$url$player       <- paste0(base,year,draft,'players/%s?embed=notes,odds,player_stats,player_match_stats,positions,trades')
  sc$url$statsPack    <- paste0(base,year,draft,'completeStatspack?player_id=')
  sc$url$playerStatus <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/playersStatus')
  sc$url$league       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/ladderAndFixtures?round=',c(1:23),'&scores=true')
  
  
  # Call API
  sc$api$league <- get_sc_data(sc$auth, sc$url$league[1])
  
  # Save common variables
  sc$var$team_id <- as.numeric(sapply(sc$api$league$ladder, function(x) x$user_team_id))
  
  # Generate API URLs
  sc$url$teams <- outer(paste0(base,year,draft,'userteams/',sc$var$team_id,'/statsPlayers?round='),c(0:sc$var$current_round),FUN="paste0")
  dim(sc$url$teams) <- NULL
  sc$url$teamTrades       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/teamtrades')
  sc$url$trades           <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/trades')
  sc$url$processedWaivers <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/processedWaivers')
  sc$url$draft            <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/recap')
  sc$url$aflFixture       <- paste0(base,year,draft,'real_fixture')
  
  return(sc)
}






# ################################################################################
# ## SUPERCOACH FUNCTIONS
# 
# ## Functions for interacting with the supercoach website and extracting data.
# 
# options(stringsAsFactors = FALSE)
# 
# suppressMessages(library(data.table))
# suppressMessages(library(zoo))
# suppressMessages(library(highcharter))
# 
# source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/secrets.R') # import supercoach authentication variables
# 
# 
# numeric_col <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}
# 
# 
# get_playerList <- function(sc){
#   
#   playerList <- get_sc_data(sc$auth, paste0(sc$url$players,'0'))
#   
#   playerData <- tibble(
#     id = unlist(lapply(playerList, function(x) x$id)),
#     feed_id = unlist(lapply(playerList, function(x) x$feed_id)),
#     player_name = NA,
#     first_name = unlist(lapply(playerList, function(x) x$first_name)),
#     last_name = unlist(lapply(playerList, function(x) x$last_name)),
#     team = unlist(lapply(playerList, function(x) x$team$abbrev)),
#     pos = unlist(lapply(playerList, function(x) paste0(lapply(x$positions, function(p) p$position), collapse = '/'))),
#     avg = unlist(lapply(playerList, function(x) x$previous_average))
#   ) %>%
#     mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
#     group_by(player_name, team) %>%
#     mutate(n = n()) %>%
#     mutate(player_name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), player_name)) %>%
#     ungroup() %>%
#     select(-n, -first_name, -last_name)
#   
#   return(playerData)
# }




# 
# 
# path <- './data/2023/raw/'
# files <- list.files(path)
# files[grep('^players',files)]
# 
# for (i in 1:24){
#   filepath <- paste0(path, 'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=', i, '.rds')
#   player_data <- readRDS(filepath)
#   names(player_data) <- lapply(player_data, function(p) p$feed_id)
#   player_data2 <- lapply(player_data, function(p){
#     
#     p <- lapply(p, function(x){
#       if(class(x) == 'list' & is.null(names(x)) & length(x)>0) {
#         names(x) <- paste0('a',1:length(x))
#       }
#       return(x)
#     })
#     
#     return(p)
#   })
#   firebaseSave(projectURL, paste0('players/S2023/R',i), player_data2)
# }
# 
# 
# x <- firebaseDownload(projectURL, 'players/S2023/')
# 
# 
# 
# ## Data Extraction Scripts
# get_sc_auth <- function(cid, tkn){
#   log('get_sc_auth')
#   
#   year <- year(Sys.Date())
#   url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/access_token')
#   
#   # POST request to get an access token
#   auth <- content(POST(
#     url = url,
#     body = list(
#       grant_type = 'social',
#       client_id = cid,
#       client_secret = '',
#       service = 'auth0',
#       token = tkn
#     ),
#     encode = 'json'
#   ))
#   
#   sc_auth <- add_headers(
#     Authorization = paste0('Bearer ', auth$access_token)
#   )
#   
#   return(sc_auth)
# }
# get_sc_data <- function(sc_auth, url){
#   log('get_sc_data')
#   log(paste0('get_sc_data: INFO: ..',str_extract(url, '(?<=/afl).+')))
#   
#   sc_data <- content(GET(
#     url = url,
#     config = sc_auth
#   ))
#   
#   if(!is.null(sc_data$message)){
#     log(paste0('get_sc_data: ERROR: ',sc_data$message))
#     sc_data <- NULL
#   }
#   
#   return(sc_data)
# }
# 
# get_sc <- function(cid, tkn){
#   log('get_sc')
#   
#   # Output Variable
#   sc <- list()
#   
#   # Get Authentication
#   sc$auth  <- get_sc_auth(cid, tkn)
#   
#   # URL components
#   base <- 'https://supercoach.heraldsun.com.au/'
#   year <- year(Sys.Date())
#   draft <- '/api/afl/draft/v1/'
#   classic <- '/api/afl/classic/v1/'
#   
#   # Generate API URLs
#   sc$url$settings <- paste0(base,year,draft,'settings')
#   sc$url$me <- paste0(base,year,classic,'me')
#   
#   # Call API
#   sc$api$settings <- get_sc_data(sc$auth, sc$url$settings)
#   sc$api$me <- get_sc_data(sc$auth, sc$url$me)
#   
#   # Save common variables
#   sc$var$user_id <- sc$api$me$id
#   
#   # Generate API URLs
#   sc$url$user <- paste0(base,year,draft,'users/',sc$var$user_id,'/stats')
#   
#   # Call API
#   sc$api$user <- get_sc_data(sc$auth, sc$url$user)
#   
#   # Save common variables
#   sc$var$season <- sc$api$settings$content$season
#   sc$var$league_id <- sc$api$user$classic$leagues[[1]]$id
#   sc$var$current_round <- sc$api$settings$competition$current_round
#   
#   # Generate API URLs
#   sc$url$players      <- paste0(base,year,draft,'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=')
#   sc$url$playerStatus <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/playersStatus')
#   sc$url$league       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/ladderAndFixtures?round=',c(1:23),'&scores=true')
#   
#   # Call API
#   sc$api$league <- get_sc_data(sc$auth, sc$url$league[1])
#   
#   # Save common variables
#   sc$var$team_id <- as.numeric(sapply(sc$api$league$ladder, function(x) x$user_team_id))
#   
#   # Generate API URLs
#   sc$url$teams <- outer(paste0(base,year,draft,'userteams/',sc$var$team_id,'/statsPlayers?round='),c(0:sc$var$current_round),FUN="paste0")
#   dim(sc$url$teams) <- NULL
#   sc$url$teamTrades       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/teamtrades')
#   sc$url$trades           <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/trades')
#   sc$url$processedWaivers <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/processedWaivers')
#   sc$url$draft            <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/recap')
#   sc$url$aflFixture       <- paste0(base,year,draft,'real_fixture')
#   
#   return(sc)
# }
# 
# 
# # Connect to SC
# sc <- get_sc(cid, tkn)
# sc_season <- sc$var$season
# sc_round <- sc$var$current_round
# 
# # Download Supercoach Data
# player_data_url <- paste0(sc$url$players, sc_round)
# player_data_raw <- get_sc_data(sc$auth, player_data_url)
# names(player_data_raw) <- unlist(lapply(player_data_raw, function(x) paste0(gsub('\\*\\*\\* ','',x$last_name),'|',x$feed_id)))
# 
# # Save to firebase
# fbRawPath <- paste0('players/raw/',sc_season,'/',sc_round)
# firebaseSave(projectURL, fbRawPath, player_data_raw)
# 
# # Clean data
# data_raw <- firebaseDownload(projectURL, fbRawPath)
# 
# data_raw1 <- lapply(data_raw, function(x){
#   x$positions <- apply(x$positions, 1, function(x) as.list(x))
#   x$positions <- paste(sort(unlist(lapply(x$positions, function(x) x$position))), collapse = ' ')
#   return(x)
# }) 
# 
# data_raw2 <- bind_rows(lapply(data_raw1, function(x) data.frame(as.list(unlist(x))))) %>%
#   mutate_if(numeric_col,as.numeric) 
# 
# data <- data_raw2 %>%
#   filter(active == TRUE) %>%
#   mutate(season = sc_season) %>%
#   rename(player_id = id) %>%
#   mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
#   rename(team_abbrev = team.abbrev) %>%
#   rename(position = positions) %>%
#   select(
#     season,
#     feed_id,
#     player_id,
#     first_name,
#     last_name,
#     player_name, 
#     team_abbrev,
#     position,
#     player_stats.round,
#     player_stats.games,
#     player_stats.points,
#     player_stats.price,
#     player_stats.total_games,
#     player_stats.avg,
#     player_stats.avg3,
#     player_stats.avg5
#   )
#   
# names(data) <- gsub('player_stats.','',names(data))
# 
# fbDataPath <- gsub('raw','data',fbRawPath)
# firebaseSave(projectURL, fbDataPath, data)
# firebaseSave(projectURL, 'players/settings', list(col_order = names(data)))
# 
# 
# 
# 
# col <- firebaseDownload(projectURL, 'players/settings/col_order')
# x <- firebaseDownload(projectURL, fbDataPath)
# x1 <- x[,col]
# 
# 
# 
# 
# 
# x <- firebaseDownload(projectURL, 'players/settings')
# 
# x$col_order
# 
# cleanPlayerData <- function(data_players, season=year(Sys.Date())){
#   log('cleanPlayerData')
#   
#   player_data <- tibble(
#     season           = season, 
#     feed_id          = as.numeric(sapply(data_players, function(x) x$feed_id)),
#     player_id        = as.numeric(sapply(data_players, function(x) x$id)),
#     first_name       = sapply(data_players, function(x) x$first_name),
#     last_name        = sapply(data_players, function(x) x$last_name),
#     player_name      = NA,
#     team_id          = as.numeric(sapply(data_players, function(x) x$team$id)),
#     team_name        = sapply(data_players, function(x) x$team$name),
#     team_abbrev      = sapply(data_players, function(x) x$team$abbrev),
#     position         = sapply(data_players, function(x) paste(sort(unlist(lapply(x$positions, function(x) x$position))), collapse = ' ')),
#     round            = as.numeric(sapply(data_players, function(x) x$player_stats[[1]]$round)),
#     avg              = round(as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$avg),NA,x$player_stats[[1]]$avg))),2),
#     avg3             = round(as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$avg3),NA,x$player_stats[[1]]$avg3))),2),
#     avg5             = round(as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$avg5),NA,x$player_stats[[1]]$avg5))),2),
#     price            = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$price),NA,x$player_stats[[1]]$price))),
#     points           = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$points),NA,x$player_stats[[1]]$points))),
#     played           = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$games),NA,x$player_stats[[1]]$games))),
#     minutes_played   = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$minutes_played),NA,x$player_stats[[1]]$minutes_played))),
#     kicks            = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$kicks),NA,x$player_stats[[1]]$kicks))),
#     handballs        = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$handballs),NA,x$player_stats[[1]]$handballs))),
#     marks            = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$marks),NA,x$player_stats[[1]]$marks))),
#     tackles          = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$tackles),NA,x$player_stats[[1]]$tackles))),
#     frees_for        = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$freekicks_for),NA,x$player_stats[[1]]$freekicks_for))),
#     frees_against    = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$freekicks_against),NA,x$player_stats[[1]]$freekicks_against))),
#     hitouts          = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$hitouts),NA,x$player_stats[[1]]$hitouts))),
#     goals            = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$goals),NA,x$player_stats[[1]]$goals))),
#     behinds          = as.numeric(sapply(data_players, function(x) ifelse(is.null(x$player_stats[[1]]$behinds),NA,x$player_stats[[1]]$behinds)))
#   ) %>%
#     mutate(player_name = paste0(substr(first_name,1,1),'.',last_name))
#   
#   return(player_data)
# }
# 
# 
# 
# 
# 
# 
# 
# # Download data
# for (i in 8:sc$var$current_round){
#   players_url <- paste0(sc$url$players, i)
#   player_data_raw <- get_sc_data(sc$auth, players_url)
#   filename <- paste0('PLAYER_DATA_2023_',formatC(i, width = 2, format = "d", flag = "0"))
#   saveRDS(player_data_raw, paste0('./data/Master/raw/',filename,'.RDS'))
# }
# 
# 
# 
# #
# path <- './data/Master/raw/'
# files <- list.files(path)
# files <- rev(sort(files))
# 
# master_data <- tibble()
# for (i in 1:length(files)){
#   
#   file <- files[i]
#   season <- substr(file, 13, 16)
#   
#   player_list <- readRDS(paste0(path,file))
#   player_data <- cleanPlayerData(player_list, season)
#   
#   master_data <- bind_rows(master_data, player_data)
# }
# 
# 
# 
# 
# url <- 'https://supercoach.heraldsun.com.au/2023/api/afl/draft/v1/completeStatspack?player_id=18'
# 
# playerStats <- get_sc_data(sc$auth, url)
# playerStats1 <- playerStats$playerStats
# 
# 
# 
# y <- bind_rows(lapply(playerStats1, function(x) data.frame(as.list(unlist(x))))) %>%
#   mutate_if(numeric_col,as.numeric) 
#   
# 
# 
# 
# numeric_col <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}
# 
# a <- z %>% 
#   mutate_if(numeric_col,as.numeric) 
# 
# 
# y <- stack(x)
# 
# y <- as_tibble(x, .name_repair = 'universal')
# 
# 
# 
# y <- bind_rows(x)
# 
# z <- flatten(x)
# 
# y <- lapply(x, unlist, use.names=FALSE)
# 
# 
# 
# 
# x <- as.data.frame(do.call("cbind", lapply(player_list, ts)))
# 
# 
# 
# 
# rawPlayerDataSC <- function(sc, vRound=NA){
#   
#   if(is.na(vRound)){
#     vRound <- sc$var$current_round
#   }
#   
#   players_url <- paste0(sc$url$players, vRound)
#   player_data_raw <- get_sc_data(sc$auth, players_url)
#   
#   return(player_data_raw)
# }
# 
# x <- rbind(player_data_raw)
# 
# x<-plyr::ldply(player_data_raw, rbind)
# 
# savePlayerData <-function(player_data){
#   
#   x <- list(
#     data = player_data,
#     settings = list(col_order = names(player_data))
#   )
#   
#   y <- firebaseSave(projectURL, 'players', x)
#   return(y)
# }
# getPlayerData <- function(){
#   players <- firebaseDownload(projectURL, 'players')
#   player_data <- data[,players$settings$col_order]
#   return(player_data)
# }
# 
# updatePlayerData <- function(player_data_new){
# 
#   player_data <- getPlayerData()
#   
#   x <- player_data_new %>%
#     select(season, round) %>%
#     distinct() %>%
#     mutate(filter = 1)
#   
#   player_data <- player_data %>%
#     left_join(x, by=c('season','round')) %>%
#     filter(is.na(filter)) %>%
#     select(-filter) %>%
#     bind_rows(player_data_new) %>%
#     arrange(desc(season), desc(round), desc(avg))
#   
#   savePlayerData(player_data)
#   
#   return(player_data)
# 
# }
# 
# player_data_raw <- rawPlayerDataSC(sc, 6)
# player_data_new <- cleanPlayerData(player_data_raw)
# 
# player_data <- updatePlayerData(player_data_new)
# 
# 
# 
# 
# # get data
# # Clean Data 
# #save data
# 
# 
# updatePlayerData <- function(sc, player_data, vSeason, vRound){
#   
#   player_data1 <- player_data %>%
#     filter(!(season == vSeason & round == vRound))
#   
#   players_url <- paste0(sc$url$players, vRound)
#   player_data_raw <- get_sc_data(sc$auth, players_url)
#   new_player_data <- extractPlayerData(player_data_raw, vSeason)
#   
#   player_data2 <- bind_rows(player_data1, new_player_data) %>%
#     arrange(desc(season), desc(round), desc(price))
#   
#   return(player_data2)
#   
# }
# 
# player_data <- updatePlayerData(sc, player_data, 2023, 7)
# 
# 
# 
# files <- list.files('./data/Master/raw/')
# 
# 
# for(i in 8:10){
#   data <- readRDS(paste0('./data/2022/raw/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=',i,'.rds'))
#   filename <- paste0('PLAYER_DATA_2023_',formatC(i, width = 2, format = "d", flag = "0"))
#   saveRDS(data, paste0('./data/Master/raw/',filename,'.RDS'))
# }
# 
# 




