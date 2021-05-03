################################################################################
## SUPERCOACH FUNCTIONS

## Functions for interacting with the supercoach website and extracting data.

options(stringsAsFactors = FALSE)
library(httr)
library(tidyverse)
library(lubridate)

source('./functions/secrets.R') # import supercoach authentication variables

get_sc_auth <- function(cid, tkn){
  
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
get_sc_settings <- function(sc_auth){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/settings')
  
  sc_settings <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_settings)
}
get_sc_me <- function(sc_auth){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/me')
  
  sc_me <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_me)
}
get_sc_user <- function(sc_auth, user_id){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/users/', user_id, '/stats')
  
  sc_user <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_user)
}
get_sc_players <- function(sc_auth, round=0){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=', round)
  
  sc_players <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_players)
}
get_sc_playerStatus <- function(sc_auth, league_id){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/playersStatus')
  
  sc_playerStatus <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_playerStatus)
}
get_sc_league <- function(sc_auth, league_id, round=0){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/ladderAndFixtures?round=', round)
  
  sc_league <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_league)
}

################################################################################
## Other APIs available:

# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/real_fixture?round=8&page=1&page_size=9998
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/userteams/2016/statsPlayers?round=8
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/teams

# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/claimWaiverPlayer/302/droppedWaiverPlayer/698
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/teamtrades
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/livedraft
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/claimedWaivers
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/trades
# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/processedWaivers

# url <- paste0('https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/claimWaiverPlayer/302/droppedWaiverPlayer/698')
# 
# test <- content(GET(
#   url = url,
#   config = sc_auth
# ))

# Will post a waiver - I wonder whether you can also grab free agents this way?
# url <- paste0('https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/claimWaiverPlayer/302/droppedWaiverPlayer/698')
# test <- content(POST(
#   url = url,
#   config = sc_auth
# ))