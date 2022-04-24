################################################################################
## SUPERCOACH FUNCTIONS

## Functions for interacting with the supercoach website and extracting data.

options(stringsAsFactors = FALSE)

suppressMessages(library(httr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(data.table))

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/secrets.R') # import supercoach authentication variables

## Data Extraction Scripts
get_sc_auth <- function(cid, tkn){
  log('get_sc_auth')
  
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
  log('get_sc_data')
  log(paste0('get_sc_data: INFO: ..',str_extract(url, '(?<=/afl).+')))
  
  sc_data <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  if(!is.null(sc_data$message)){
    log(paste0('get_sc_data: ERROR: ',sc_data$message))
    sc_data <- NULL
  }
  
  return(sc_data)
}




get_sc_settings <- function(sc_auth){
  log('get_sc_settings')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/settings')
  
  sc_settings <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_settings)
}
get_sc_me <- function(sc_auth){
  log('get_sc_me')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/me')
  
  sc_me <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_me)
}
get_sc_user <- function(sc_auth, user_id){
  log('get_sc_user')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/classic/v1/users/', user_id, '/stats')
  
  sc_user <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_user)
}
get_sc_players <- function(sc_auth, round=0){
  log('get_sc_players')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=', round)
  
  sc_players <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_players)
}
get_sc_playerStatus <- function(sc_auth, league_id){
  log('get_sc_playerStatus')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/playersStatus')
  
  sc_playerStatus <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_playerStatus)
}
get_sc_league <- function(sc_auth, league_id, round=0){
  log('get_sc_league')
  
 # league_id <- 651660
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/ladderAndFixtures?round=', round,'&scores=true')
  
  sc_league <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_league)
}
get_sc_team <- function(sc_auth, team_id, round=0){
  log('get_sc_team')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/userteams/', team_id,'/statsPlayers?round=',round)
  
  sc_team <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_team)
}
get_sc_teamtrades <- function(sc_auth,league_id){
  log('get_sc_teamtrades')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/teamtrades')
  
  sc_trades <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_trades)
}
get_sc_trades <- function(sc_auth,league_id){
  log('get_sc_trades')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/trades')
  
  sc_trades <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_trades)
}
get_sc_processedWaivers <- function(sc_auth,league_id){
  log('get_sc_processedWaivers')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/processedWaivers')
  
  sc_processedWaivers <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_processedWaivers)
}
get_sc_draft <- function(sc_auth,league_id,user_id=2016){
  log('get_sc_draft')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/', year, '/api/afl/draft/v1/leagues/',league_id,'/userteam/',user_id,'/livedraft')
  
  sc_trades <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(sc_trades)
}
get_afl_fixture <- function(sc_auth, round){
  log('get_afl_fixture')
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/',year,'/api/afl/draft/v1/real_fixture?round=',round)
  
  afl_fixture <- content(GET(
    url = url,
    config = sc_auth
  ))
  
  return(afl_fixture)
}



## Data Cleansing Scripts
get_sc_player_data <- function(sc_players){
  log('get_sc_player_data')
  
  player_data <- tibble(
    feed_id          = as.numeric(sapply(sc_players, function(x) x[['feed_id']])),
    player_id        = as.numeric(sapply(sc_players, function(x) x[['id']])),
    first_name       = sapply(sc_players, function(x) x[['first_name']]),
    last_name        = sapply(sc_players, function(x) x[['last_name']]),
    team_abbrev      = sapply(sc_players, function(x) x[['team']][['abbrev']]),
    pos_1            = sapply(sc_players, function(x) x[['positions']][[1]][['position']]),
    pos_2            = sapply(sc_players, function(x) ifelse(length(x[['positions']])>1, x[['positions']][[2]][['position']],NA)),
    round            = as.numeric(sapply(sc_players, function(x) x[['player_stats']][[1]][['round']])),
    projected_points = as.numeric(sapply(sc_players, function(x) ifelse(is.null(x[['player_stats']][[1]][['ppts']]), NA,x[['player_stats']][[1]][['ppts']]))),
    points           = as.numeric(sapply(sc_players, function(x) ifelse(length(x[['player_match_stats']])==0,        NA,x[['player_match_stats']][[1]][['points']]))),
    avg              = as.numeric(sapply(sc_players, function(x) ifelse(is.null(x[['player_stats']][[1]][['avg']]),  NA,x[['player_stats']][[1]][['avg']]))),
    avg3             = as.numeric(sapply(sc_players, function(x) ifelse(is.null(x[['player_stats']][[1]][['avg3']]), NA,x[['player_stats']][[1]][['avg3']]))),
    avg5             = as.numeric(sapply(sc_players, function(x) ifelse(is.null(x[['player_stats']][[1]][['avg5']]), NA,x[['player_stats']][[1]][['avg5']]))),
    price            = as.numeric(sapply(sc_players, function(x) ifelse(is.null(x[['player_stats']][[1]][['price']]),NA,x[['player_stats']][[1]][['price']])))
  )
  
  return(player_data)
}
get_sc_team_data <- function(sc_league){
  log('get_sc_team_data')
  
  team_data <- tibble()
  for(i in 1:length(sc_league$ladder)){
    
    this_team <- tibble()
    for(type in c('scoring','nonscoring')){
      
      tData <- tibble(
        round        = as.numeric(sapply(sc_league$ladder[[i]]$userTeam$scores[[type]], function(x) x[['round']])),
        player_id    = as.numeric(sapply(sc_league$ladder[[i]]$userTeam$scores[[type]], function(x) x[['player_id']])),
        picked       = as.logical(sapply(sc_league$ladder[[i]]$userTeam$scores[[type]], function(x) x[['picked']])),
        position     = sapply(sc_league$ladder[[i]]$userTeam$scores[[type]], function(x) x[['position']]),
        user_team_id = as.numeric(sapply(sc_league$ladder[[i]]$userTeam$scores[[type]], function(x) x[['user_team_id']]))
      ) %>%
        mutate(team  = sc_league$ladder[[i]]$userTeam$teamname) %>%
        mutate(coach = sc_league$ladder[[i]]$userTeam$user$first_name) %>%
        mutate(type  = type) %>%
        dplyr::select(round,
               user_team_id,
               team,
               coach,
               player_id,
               picked,
               type,
               position)
      
      this_team <- bind_rows(this_team, tData)
      
    }
    
    team_data <- bind_rows(team_data, this_team)
    
  }
    
  return(team_data)
  
}
get_sc_ladder_data <- function(sc_league){
  log('get_sc_ladder_data')
  
  ladder_data <- tibble(
    team_id        = as.numeric(sapply(sc_league$ladder, function(x) x$user_team_id)),
    teamname       = sapply(sc_league$ladder, function(x) x$userTeam$teamname),
    coach          = sapply(sc_league$ladder, function(x) x$userTeam$user$first_name),
    round          = as.numeric(sapply(sc_league$ladder, function(x) x$round)),
    position       = as.numeric(sapply(sc_league$ladder, function(x) x$position)),
    points         = as.numeric(sapply(sc_league$ladder, function(x) x$points)),
    wins           = as.numeric(sapply(sc_league$ladder, function(x) x$wins)),
    draws          = as.numeric(sapply(sc_league$ladder, function(x) x$draws)),
    losses         = as.numeric(sapply(sc_league$ladder, function(x) x$losses)),
    points_for     = as.numeric(sapply(sc_league$ladder, function(x) x$points_for)),
    points_against = as.numeric(sapply(sc_league$ladder, function(x) x$points_against))
  ) %>%
    mutate(pcnt = round(points_for/points_against*100,2))
  
  return(ladder_data)
}
get_sc_fixture_data <- function(sc_league){
  log('get_sc_fixture_data')
  
  home_data <- tibble(
    round            = as.numeric(sapply(sc_league$fixture, function(x) x$round)),
    fixture          = as.numeric(sapply(sc_league$fixture, function(x) x$fixture)),
    team_id          = as.numeric(sapply(sc_league$fixture, function(x) x$user_team1$id)),
    team             = sapply(sc_league$fixture, function(x) x$user_team1$teamname),
    coach            = sapply(sc_league$fixture, function(x) x$user_team1$user$first_name),
    team_score       = as.numeric(sapply(sc_league$fixture, function(x) ifelse(is.null(x$user_team1$stats[[1]]$points),NA,x$user_team1$stats[[1]]$points))),
    opponent_team_id = as.numeric(sapply(sc_league$fixture, function(x) x$user_team2$id)),
    opponent_team    = sapply(sc_league$fixture, function(x) x$user_team2$teamname),
    opponent_coach   = sapply(sc_league$fixture, function(x) x$user_team2$user$first_name),
    opponent_score   = as.numeric(sapply(sc_league$fixture, function(x) ifelse(is.null(x$user_team2$stats[[1]]$points),NA,x$user_team2$stats[[1]]$points)))
  )
  
  away_data <- home_data 
  names(away_data) <- names(home_data)[c(1:2,7:10,3:6)]
  
  fixture_data <- bind_rows(home_data,away_data) %>%
    mutate(team_score = ifelse(team_score==0,NA,team_score)) %>%
    mutate(opponent_score = ifelse(opponent_score==0,NA,opponent_score)) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0))

  return(fixture_data)
}
get_afl_fixture_data <- function(afl_fixture){
  log('get_afl_fixture_data')
  
  home <- tibble(
    season       = as.numeric(sapply(afl_fixture, function(x) x$season)),
    round        = as.numeric(sapply(afl_fixture, function(x) x$round)),
    game_num     = as.numeric(sapply(afl_fixture, function(x) x$id)),
    kickoff      = with_tz(as_datetime(sapply(afl_fixture, function(x) x$kickoff)), "Australia/Melbourne"),
    status       = sapply(afl_fixture, function(x) x$status),
    team1        = sapply(afl_fixture, function(x) x$team1$name),
    team1_abbrev = sapply(afl_fixture, function(x) x$team1$abbrev),
    team2        = sapply(afl_fixture, function(x) x$team2$name),
    team2_abbrev = sapply(afl_fixture, function(x) x$team2$abbrev),
    home_flag    = 1
  )
  
  away <- home %>%
    mutate(home_flag = 0)
  
  names(away) <- names(home)[c(1:5,8:9,6:7,10)]
  
  afl_fixture_data <- bind_rows(home,away) %>%
    arrange(kickoff, desc(home_flag))
  
  return(afl_fixture_data)
}

get_ff_fixture_data <- function(vSeason=NA, vRound=NA){
  log('get_ff_fixture_data')
  
  ff_fixture <- as_tibble(fread('http://www.fanfooty.com.au/resource/draw.php'))
  colnames(ff_fixture) <- c('game_id', 
                             'year', 
                             'competition', 
                             'round', 
                             'gametime_AET', 
                             'day', 
                             'home_team', 
                             'away_team', 
                             'ground', 
                             'timeslot', 
                             'TV_coverage', 
                             'home_supergoals', 
                             'home_goals', 
                             'home_behinds', 
                             'home_points', 
                             'away_supergoals', 
                             'away_goals', 
                             'away_behinds', 
                             'away_points', 
                             'match_status')
  
  if(!is.na(vSeason)){
    ff_fixture <- ff_fixture %>%
      filter(year==vSeason)
  }
  
  if(!is.na(vRound)){
    ff_fixture <- ff_fixture %>%
      filter(round==vRound)
  }
  
  ff_fixture$home_team[ff_fixture$home_team == 'GWS'] <- 'GWS Giants'
  ff_fixture$away_team[ff_fixture$away_team == 'GWS'] <- 'GWS Giants'
  ff_fixture$home_team[ff_fixture$home_team == 'Brisbane Lions'] <- 'Brisbane'
  ff_fixture$away_team[ff_fixture$away_team == 'Brisbane Lions'] <- 'Brisbane'
  
  return(ff_fixture)
}
get_ff_game_data <- function(game_id){
  log('get_ff_game_data')
  
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
  
  game_data$feed_id <- as.numeric(game_data$feed_id)
  game_data$supercoach <- as.numeric(game_data$supercoach)
  game_data$time_on_ground <- as.numeric(game_data$time_on_ground)
  
  return(game_data)
  
}

## Data Processing Scripts
get_sc <- function(cid, tkn){
  log('get_sc')
  
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
  sc$var$league_id <- sc$api$user$classic$leagues[[1]]$id
  sc$var$current_round <- sc$api$settings$competition$current_round
  sc$var$first_round <- sc$api$user$classic$leagues[[1]]$options$round_leagues_start
  sc$var$last_round <- sc$api$user$classic$leagues[[1]]$options$round_leagues_end
  
  # Generate API URLs
  sc$url$players      <- paste0(base,year,draft,'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=',c(1:sc$var$current_round))
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
  sc$url$draft            <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/userteam/',sc$var$user_id,'/livedraft')
  sc$url$aflFixture       <- paste0(base,year,draft,'real_fixture')
  
  return(sc)
}
get_player_data <- function(sc, round=NA){
  log('get_player_data')

  # Get URLs
  url_players <- sc$url$players
  url_league <- sc$url$league[1:sc$var$current_round]
  
  # Filter for round if required
  if(!is.na(round)){
    str<-paste0('round=',round)
    url_players <- url_players[grepl(str, url_players)]
    url_league <- url_league[grepl(str, url_league)]
  }

  # Create data placeholders
  player_data <- tibble()
  team_data <- tibble()
  
  # Download player data
  for(url in url_players){
    sc_players <- get_sc_data(sc$auth, url)
    p_data <- get_sc_player_data(sc_players)
    player_data <- bind_rows(player_data, p_data)
  }
  
  # Download team data
  for(url in url_league){
    sc_league <- get_sc_data(sc$auth, url)
    t_data <- get_sc_team_data(sc_league)
    team_data <- bind_rows(team_data, t_data)
  }
  
  # Join teams to player data
  player_data <- player_data %>%
    left_join(team_data, by=c('round', 'player_id')) %>%
    filter(round!=0)
  
  return(player_data)
}
get_league_data <- function(sc, round=NA){
  log('get_league_data')
  
  # Get URLs
  url_league <- sc$url$league
  
  # Filter for round if required
  if(!is.na(round)){
    str<-paste0('round=',round)
    url_league <- url_league[grepl(str, url_league)]
  }
  
  # Create data placeholders
  leauge_data <- tibble()
  
  # Download team data
  for(url in url_league){
    
    url_round <- as.numeric(str_extract(url,'(?<=round=).+(?=&)')) 
      
    if(url_round <= max(sc$var$last_round, sc$var$current_round)){ # to account for finals when available
      
      sc_league <- get_sc_data(sc$auth, url)
      
      fData <- get_sc_fixture_data(sc_league)
      
      if(url_round <= sc$var$current_round){
        
        lData <- get_sc_ladder_data(sc_league) %>%
          dplyr::select(-teamname, -coach, -round)
        
        data <- fData %>%
          left_join(lData, by=c('team_id'))
        
      } else {
        data <- fData
      }
      
      # bind
      leauge_data <- bind_rows(leauge_data, data)
    }
  }
 
  return(leauge_data)
}

sc_download_all <- function(cid, tkn){
  
  yr <- year(Sys.Date())
  path <- paste0('./data/2021/raw')
  
  sc_auth <- get_sc_auth(cid, tkn)
  
  sc_settings <- get_sc_settings(sc_auth)
  saveRDS(sc_settings, paste0(path,'/sc_settings.rds'))
  
  sc_me <- get_sc_me(sc_auth)
  saveRDS(sc_me, paste0(path,'/sc_me.rds'))
  
  sc_user <- get_sc_user(sc_auth, sc_me$id)
  saveRDS(sc_user, paste0(path,'/sc_user.rds'))
  
  rnd <- 23
  league_id <- sc_user$draft[[1]]$leagues[[1]]$id
  for(i in 1:rnd) {
    sc_players <- get_sc_players(sc_auth, i)
    saveRDS(sc_players, paste0(path,'/sc_players_',i,'.rds'))
    
    sc_league <- get_sc_league(sc_auth, league_id, i)
    saveRDS(sc_league, paste0(path,'/sc_league_',i,'.rds'))
    
    afl_fixture <- get_afl_fixture(sc_auth, i)
    saveRDS(afl_fixture, paste0(path,'/afl_fixture_',i,'.rds'))
  }
  
  sc_teamtrades <- get_sc_teamtrades(sc_auth,league_id)
  saveRDS(sc_teamtrades, paste0(path,'/sc_teamtrades.rds'))
  
  sc_trades <- get_sc_trades(sc_auth,league_id)
  saveRDS(sc_trades, paste0(path,'/sc_trades.rds'))
  
  sc_processedWaivers <- get_sc_processedWaivers(sc_auth,league_id)
  saveRDS(sc_processedWaivers, paste0(path,'/sc_processedWaivers.rds'))
  
  sc_draft <- get_sc_draft(sc_auth,league_id)
  saveRDS(sc_draft, paste0(path,'/sc_draft.rds'))
  
}

## Other Handy Functions
get_sc_logo <- function(logoSize=38, shirttype, shortcolor, basecolor, secondcolor){
  
  # Get
  logoBgSize <- logoSize*8
  logoShortsXY <- as.numeric(unlist(str_split(shortcolor,',')))
  logoJumperXY <- as.numeric(unlist(str_split(basecolor,',')))
  logoPatternXY <- as.numeric(unlist(str_split(secondcolor,',')))
  logoPatternType <- as.numeric(shirttype)
  
  logo <- div(
    id='scLogo',
    class='logoShorts',
    style=paste0("
      height:", logoSize,"px;
      width:", logoSize,"px;
      background-image:url('shirt_shorts.png');
      background-size:",logoBgSize,"px;
      background-position:",-logoSize*logoShortsXY[1],"px ",-logoSize*logoShortsXY[2],"px;
    "),
    
    div(
      class='logoJumper',
      style=paste0("
        height:100%;
        width:100%;
        background-image:url('shirt_jumper.png');
        background-size:",logoBgSize,"px;
        background-position:",-logoSize*logoJumperXY[1],"px ",-logoSize*logoJumperXY[2],"px;
      "),
      
      div(
        class='logoPattern',
        style=paste0("
          height:100%;
          width:100%;
          background-image:url('shirt_pattern_",logoPatternType,".png');
          background-size:",logoBgSize,"px;
          background-position:",-logoSize*logoPatternXY[1],"px ",-logoSize*logoPatternXY[2],"px;
        "),
        div(
          class='logoOverlay',
          style=paste0("
          height:100%;
          width:100%;
          background-image:url('mask_overlay.png');
          background-size:",logoSize,"px;
        ")
        )
      )
    )
  )
  
}
log <- function(...){
  time <- format(with_tz(as_datetime(Sys.time()), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S")
  msg <- paste0(time,': ', ...)
  message(msg)
}

## Other Handy Variables
team_colours <- list(
  list(
    name = 'Anthony',
    nickname = 'Melons',
    logo='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',
    color = '#e2356d'
  ),
  list(
    name='James',
    nickname = 'Garter',
    logo='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',
    color = '#a90000'
  ),
  list(
    name='Jordan',
    nickname='Jmerc',
    logo='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',
    color = '#f98d33'
  ),
  list(
    name='Lester',
    nickname='Lester',
    logo='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',
    color = '#b486f8'
  ),
  list(
    name='Luke',
    nickname='Kappaz',
    logo='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',
    color = '#e93f33'
  ),
  list(
    name='Mark',
    nickname='Richo',
    logo='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',
    color = '#023101'
  ),
  list(
    name='Paul',
    nickname='Pmac',
    logo='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',
    color = '#f8e71c'
  ),
  list(
    name='Simon',
    nickname='Chief',
    logo='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',
    color = '#000000'
  )
)



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

# url <- paste0('https://supercoach.heraldsun.com.au/2021/api/afl/draft/v1/leagues/575/trades')
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

