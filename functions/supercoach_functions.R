################################################################################
## SUPERCOACH FUNCTIONS

## Functions for interacting with the supercoach website and extracting data.

options(stringsAsFactors = FALSE)
library(httr)
library(tidyverse)
library(lubridate)
library(tictoc)

source('/Users/paulmcgrath/Github/Addicts-Supercoach-League/functions/secrets.R') # import supercoach authentication variables

## Data Extraction Scripts
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
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/ladderAndFixtures?round=', round,'&scores=true')
  
  sc_league <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_league)
}
get_sc_team <- function(sc_auth, team_id, round=0){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/userteams/', team_id,'/statsPlayers?round=',round)
  
  sc_team <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_team)
}

## Data Cleansing Scripts
get_sc_player_data <- function(sc_players){
  
  raw <- lapply(sc_players, unlist)
  raw <- bind_rows(lapply(raw, as.data.frame.list))
  
  player_data <- tibble(
    feed_id          = as.numeric(raw$feed_id),
    player_id        = as.numeric(raw$id),
    first_name       = raw$first_name,
    last_name        = raw$last_name,
    team_abbrev      = raw$team.abbrev,
    pos_1            = raw$positions.position,
    pos_2            = raw$positions.position.1,
    round            = as.numeric(raw$player_stats.round),
    projected_points = rep(NA, length(raw$feed_id)),
    points           = as.numeric(raw$player_match_stats.points),
    avg              = as.numeric(raw$player_stats.avg),
    avg3             = as.numeric(raw$player_stats.avg3),
    avg5             = as.numeric(raw$player_stats.avg5),
    price            = as.numeric(raw$player_stats.price)
  )
  
  if('player_stats.ppts' %in% names(raw)){
    player_data$projected_points = as.numeric(raw$player_stats.ppts)
  }
  
  return(player_data)
}
get_sc_team_data <- function(sc_league){
  
  team_data <- tibble()
  for(i in 1:length(sc_league$ladder)){
    
    scoring <- lapply(sc_league$ladder[[i]]$userTeam$scores$scoring, unlist)
    scoring <- bind_rows(lapply(scoring, as.data.frame.list)) %>%
      mutate(type = 'scoring') %>%
      mutate(team = sc_league$ladder[[i]]$userTeam$teamname) %>%
      mutate(coach = sc_league$ladder[[i]]$userTeam$user$first_name) %>%
      select(round,
             user_team_id,
             team,
             coach,
             player_id,
             picked,
             type,
             position)
    
    nonscoring <- lapply(sc_league$ladder[[i]]$userTeam$scores$nonscoring, unlist)
    nonscoring <- bind_rows(lapply(nonscoring, as.data.frame.list)) %>%
      mutate(type = 'non-scoring') %>%
      mutate(team = sc_league$ladder[[i]]$userTeam$teamname) %>%
      mutate(coach = sc_league$ladder[[i]]$userTeam$user$first_name) %>%
      select(round,
             user_team_id,
             team,
             coach,
             player_id,
             picked,
             type,
             position)
    
    team_data <- team_data %>%
      bind_rows(scoring) %>%
      bind_rows(nonscoring)
  }
  
  team_data <- team_data %>%
    mutate(round = as.numeric(round)) %>%
    mutate(player_id = as.numeric(player_id)) %>%
    mutate(user_team_id = as.numeric(user_team_id)) %>%
    mutate(picked = as.logical(picked))
    
  return(team_data)
  
}
get_sc_ladder_data <- function(sc_league){
  raw <- sc_league$ladder
  
  raw <- lapply(raw, unlist)
  raw <- bind_rows(lapply(raw, as.data.frame.list))
  
  ladder_data <- tibble(
    user_team_id   = as.numeric(raw$user_team_id),
    teamname       = raw$userTeam.teamname,
    coach          = raw$userTeam.user.first_name,
    round          = as.numeric(raw$round),
    wins           = as.numeric(raw$wins),
    draws          = as.numeric(raw$draws),
    losses         = as.numeric(raw$losses),
    points         = as.numeric(raw$points),
    points_for     = as.numeric(raw$points_for),
    points_against = as.numeric(raw$points_against),
    position       = as.numeric(raw$position),
    round_points   = as.numeric(raw$userTeam.stats.points),
    total_points   = as.numeric(raw$userTeam.stats.total_points)
  )
  
  return(ladder_data)
}
get_sc_fixture_data <- function(sc_league){
  
  raw <- lapply(sc_league$fixtures, unlist)
  raw <- bind_rows(lapply(raw, as.data.frame.list))
  
  home_data <- raw %>%
    rename(team_id = user_team1.id,
           team = user_team1.teamname,
           coach = user_team1.user.first_name,
           opponent_team_id = user_team2.id,
           opponent_team = user_team2.teamname,
           opponent_coach = user_team2.user.first_name
    )
  
  home_data <- home_data[,c(
    'round',
    'fixture',
    'team_id',
    'team',
    'coach',
    'opponent_team_id',
    'opponent_team',
    'opponent_coach'
  )]
  
  # Join on scores if they exist
  if('user_team1.stats.points' %in% names(raw)){
    scores <- raw %>%
      rename(team_score = user_team1.stats.points,
             opponent_score = user_team2.stats.points) %>%
      select(team_score, opponent_score)
    
    home_data <- bind_cols(home_data, scores)
  } else {
    home_data$team_score <- NA
    home_data$opponent_score <- NA
  }

  away_data <- home_data[,c(
    'round',
    'fixture',
    'opponent_team_id',
    'opponent_team',
    'opponent_coach',
    'team_id',
    'team',
    'coach',
    'opponent_score',
    'team_score'
  )]
  
  names(away_data) <- names(home_data)
  
  fixture_data <- as_tibble(bind_rows(home_data, away_data)) %>%
    mutate(team_id = as.numeric(team_id)) %>%
    mutate(opponent_team_id = as.numeric(opponent_team_id)) %>%
    mutate(round = as.numeric(round)) %>%
    mutate(fixture = as.numeric(fixture)) %>%
    mutate(team_score = as.numeric(team_score)) %>%
    mutate(opponent_score = as.numeric(opponent_score))
    
  return(fixture_data)
}

## Data Processing Scripts
get_player_data <- function(cid, tkn, round=NA){
  tic()
  
  # Get authentication
  sc_auth <- get_sc_auth(cid, tkn)
  
  # Get round information
  if(is.na(round)){
    sc_settings <- get_sc_settings(sc_auth)
    round <- c(1:sc_settings$competition$current_round)
  }
  
  # Get user id
  sc_me <- get_sc_me(sc_auth)
  user_id <- sc_me$id
  
  # Get draft league id
  sc_user <- get_sc_user(sc_auth, user_id)
  league_id <- sc_user$draft[[1]]$leagues[[1]]$id
  
  # Create data placeholders
  player_data <- tibble()
  team_data <- tibble()
  
  # Gather data for selected rounds
  for(i in round){
    
    # Gather player data
    sc_players <- get_sc_players(sc_auth, i)
    p_data <- get_sc_player_data(sc_players)
    player_data <- bind_rows(player_data, p_data)
    
    # Gather league data
    sc_league <- get_sc_league(sc_auth, league_id, i)
    
    # Gather team data
    t_data <- get_sc_team_data(sc_league)
    team_data <- bind_rows(team_data, t_data)
  }
  
  # Join teams to player data
  player_data <- player_data %>%
    left_join(team_data, by=c('round', 'player_id'))
  
  toc()
  return(player_data)
}
get_fixture_data <- function(cid, tkn, round=NA){
  tic()
  
  # Get authentication
  sc_auth <- get_sc_auth(cid, tkn)
  
  # Get user id
  sc_me <- get_sc_me(sc_auth)
  user_id <- sc_me$id
  
  # Get draft league id
  sc_user <- get_sc_user(sc_auth, user_id)
  league_id <- sc_user$draft[[1]]$leagues[[1]]$id

  # If round is NA get whole season
  if(is.na(round)){
    season_length <- sc_user$draft[[1]]$leagues[[1]]$options$game_finals_round-1
    round <- c(1:season_length)
  }
  
  # Create data placeholders
  fixture_data <- tibble()
  
  # Gather data for selected rounds
  for(i in round){
    
    # Gather fixture data
    sc_league <- get_sc_league(sc_auth, league_id, i)
    f_data <- get_sc_fixture_data(sc_league)
    fixture_data <- bind_rows(fixture_data, f_data)
  
  }
  
  toc()
  return(fixture_data)
}

################################################################################
## Other APIs available:

# https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/real_fixture?round=8&page=1&page_size=9998
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
# no you cant get FA this way
# player_data %>%
#   filter(round == 8) %>%
#   filter((first_name == 'Zak' & last_name=='Jones') | 
#         (first_name == 'Sebastian' & last_name=='Ross') |
#           last_name == 'Coniglio')
# 
# sc_auth <- get_sc_auth(cid,tkn)
# url <- paste0('https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/userteam/2016/claimWaiverPlayer/626/droppedWaiverPlayer/639')
# test <- content(POST(
#   url = url,
#   config = sc_auth
# ))

################################################################################
## WIP Testing

# sc_auth <- get_sc_auth(cid, tkn)
# 
# sc_settings <- get_sc_settings(sc_auth)
# round <- sc_settings$competition$current_round
# 
# sc_me <- get_sc_me(sc_auth)
# user_id <- sc_me$id
# 
# sc_user <- get_sc_user(sc_auth, user_id)
# league_id <- sc_user$draft[[1]]$leagues[[1]]$id
# 
# sc_players <- get_sc_players(sc_auth, round)
# 
# sc_playerStatus <- get_sc_playerStatus(sc_auth, league_id)
# 
# sc_league <- get_sc_league(sc_auth, league_id, round)
# 
# sc_team <- get_sc_team(sc_auth, 2016, round)
# 
# player_data <- get_sc_player_data(sc_players)
# player_data
# 
# team_data <- get_sc_team_data(sc_league)
# team_data
# 
# ladder_data <- get_sc_ladder_data(sc_league)
# ladder_data
# 
# fixture_data <- get_sc_fixture_data(sc_league)
# fixture_data
# 
# fixture_data <- get_fixture_data(cid,tkn)
