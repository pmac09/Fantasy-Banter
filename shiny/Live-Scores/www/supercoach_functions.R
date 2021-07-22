################################################################################
## SUPERCOACH FUNCTIONS

## Functions for interacting with the supercoach website and extracting data.

options(stringsAsFactors = FALSE)

suppressMessages(library(httr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(data.table))

source('www/secrets.R') # import supercoach authentication variables

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
    status           = sapply(sc_players, function(x) x[['played_status']][['status']]),
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
        select(round,
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
  
  # Get settings 
  sc$settings <- get_sc_settings(sc$auth)
  
  # Get me for user ID
  sc$me <- get_sc_me(sc$auth)
  
  # Get user for league ID
  sc$user <- get_sc_user(sc$auth, sc$me$id)
  
  return(sc)
}
get_player_data <- function(sc, round=NA){
  log('get_player_data')

  # Get all rounds if round not provided
  if(is.na(round)) round <- c(1:sc$settings$competition$current_round)
  
  # Get draft league id
  league_id <- sc$user$draft[[1]]$leagues[[1]]$id
  
  # Create data placeholders
  player_data <- tibble()
  team_data <- tibble()
  
  # Gather data for selected rounds
  for(i in round){
    log(paste0('Round ', i))
    
    # Gather player data
    sc_players <- get_sc_players(sc$auth, i)
    p_data <- get_sc_player_data(sc_players)
    player_data <- bind_rows(player_data, p_data)
    
    # Gather league data
    sc_league <- get_sc_league(sc$auth, league_id, i)
    
    # Gather team data
    t_data <- get_sc_team_data(sc_league)
    team_data <- bind_rows(team_data, t_data)
  }
  
  # Join teams to player data
  player_data <- player_data %>%
    left_join(team_data, by=c('round', 'player_id'))
  
  return(player_data)
}
get_league_data <- function(sc, round=NA){
  log('get_league_data')
  
  # Get all rounds if round not provided
  if(is.na(round)) round <- c(1:(sc$user$draft[[1]]$leagues[[1]]$options$game_finals_round-1))
  
  # Get draft league id
  league_id <- sc$user$draft[[1]]$leagues[[1]]$id
  
  # Create data placeholders
  leauge_data <- tibble()
  
  # Gather data for selected rounds
  for(i in round){
    log(paste0('Round ', i))
    
    # Gather fixture data
    sc_league <- get_sc_league(sc$auth, league_id, i)
    
    # Transform
    fData <- get_sc_fixture_data(sc_league)
    
    if(i <= sc$settings$competition$current_round){
      
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
  
  return(leauge_data)
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
    logo='iVBORw0KGgoAAAANSUhEUgAAACYAAAAmCAYAAACoPemuAAAAAXNSR0IArs4c6QAAAFBlWElmTU0AKgAAAAgAAgESAAMAAAABAAEAAIdpAAQAAAABAAAAJgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAJqADAAQAAAABAAAAJgAAAACWLw5FAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgoZXuEHAAAJ60lEQVRYCY2YbYxUVxnHn7kzOzuzLzC7y74BK9BCcUOtxBJejET6QT4YJZg0S9NG09QGidh+aIIRg4KmitUEo2ICrVEoIdWN0ab2QwETaLSRhJeKsEuplgJLYdmFnZ19m9d7r///c+6ZubvsAje5c855zrnn/O7/ec4z996ITHP4vh+Zxhw2xV490/fEex8PvHpzNLtgYDwv/SM54UVt9Qlpq4ujTF5d05HavHnV4uMwl8IXT61HIhH/LttUw73a3T098V/94/bXzt4c3odxKYJExBdMbEpt04bDp511b/ixtsYt317b8teuZcsK7HqQQ+e438Djvh97bs+Rb32Syf46Io5TBwE63awk/aJkYnEZjlVL3olKxPck6ZWk0S3KbDcvOYnKh7EamRAHoJ7XXpd8ce+2r+x/IhK5p4LkuS/Yyn3HV5z9JHPMiUjqkeK4NEY8uZiYJR4ujWI9KoYCUCyhUtAGiNqiviuLC2My5EflcqwWgDL8mdbUl068sP70vQSZEWzXrl3O/upVLw9O5LfP8/LSJgW5GJ+ti0dBSRgHoaFAdFkIjHWeCodbULcCdCFu7JYkpN+JS1Mivvtp9/wOrIMBdx/TgnV3d0e/3pP4s+t5G5eXRuRqVZ3kojGFijkOFvUAJVAMsaUQ1M8qZ+oWxirHUjxPEnBzk1uQnmi9VDnOm79ZXvVkV1eXOxWNNz7p4I78Rm/icMl1N64pDklvvF6yiB8OjBKKAIByGPCEQtucxq16p4FahCOMlmrzJYvR1zHb5/KDUnRLG7eezR6eLgvcBda++8i2Ysnd9HlAna5uQqgDSkGMu8IQRhWCARRnBcrC0p3GpQT0PVd8t4SqJ/+VuCzPDUoJay3+4eFtk9Sxc1nj6t8e6zzVP967vJiR3mrEE1SJ4mRQm7gK1NILJ7tO7zBwF+EFEKakYoBy4a2gVED242zH5vggNls6mxs6//X9Jz+wLGXFGFeAOjLPKyCm6jEpgAilahm3let0n7rJLG6UMjGksQQAhQrcZ12JjIHuims9z5fBYkmaS1npuTV4lAwEo2sd69/t11MbANLR6uVkIhq4D4MqcMZVJq4mx9NdNgWoxBfVokoW2irGMothqewQaTq2nxvdMEmxbt+PXktnDy0pjskFTQkEMqfuwADQKlMBMbGlilAJulDVMuC0WwjaLaBVzqjnSZ/nSEcuI/3p8UPd3Uh4XI8/e157dz28VptCJrfuYoeeoDEBb9xHuHJbQQyMqqFuMjGlbcKoUgREne0QLPs8bAbak9kRxmXtzy++vp5MuvaZ68MHagB1oTpVhgm70AGtBbLZ3ahmFbJqBbGHhaxaprRto5q1eepiT7xSSa5hSDXyW+/1gQPkcvadvrIW2amlEwHoI0+pYuwpKwWFcJc2qRpAqoZDg9ssynRQsQUKUcFARaOcUVCVAox1KedB3pT5E2kM91t+9vbJtc57V+4cYGwk8cdsXEd1TCqganYxW3ISglpXWbspw8rgfzpwpboQbjMx5cNj3AiV+KM7aXPy4xjjysn/XT8Q6x/NLSDAcFV1RS2IYeLIqKXqBUC0G6UAqHWTGjihbVfUCeJqCgj77Ukg41JXRjQOfbkxNLwgdmvMPOAdGjojC0vj9K74AOWJ+xeX6qDOPzO2eZYwge1jP0ZICaWHky5hv45hG/ViYMN/b7lOewHupI0lx9yOVMnL9W0ykB4RKJbXB7oWP68AXMSeXKhcD6CsjaWtE451heG4UNsC2n62CcMb4F+T3gjqtFUjh3quJ7dHxyVmghyz0RFQhpSqDCZX5dBThoNNIbh4YOeCnJSKMhVoHaW1a4k2AdhXBkQ7rKRR2sYdNmFbXTWQRPqjCQWwC4YBrSK2tIvq3RMGJwNboYLS9pUVop2AdlxwHa+hjWAjyK1UsbE2ITGCXR4ak+cblss1PHcxgCelB0xgd6FqiQtBQXk0hTCb8xqWZrcxLxUVFEbNUQx0DXAdg7hCTHGMRzvjjHWUjV5R550zq06cdrzNUDE+p7NUKMKF6gbEwNgdiVl1ErYr+cjkKTMGgACxfSzZNoB89DFQdgzbNVwUx9ym2c85azoaN3OiHLMYOhUIneXYQx8I1G6hDDysWIjQdutPLS20gSEU8xWheI0FDRTDXG5VklwDK1vmv+E8vmrRcTxrDV+KJqUKVNaVnFRvAAuXXUnIwI2EKEOVAc2CFqjyp22gKupZtQjKPq7hSX9NA+SJ/GHns+sKsceRgj7bOnvLv2+m//ha+pysK9wxwczcBZAi7w63wTpPONBs9aCtgYsxNqg53uywIGdhYd19Qcm6nkHuKjLeMNeVRIMcjD4sn57f/BaW8PkvJHwZhVLe3uRCbHvUgpTBHcODF5bhYKOVZ3gX2t0VttFlhAj3KTRsttT0gfaJxiW61FOr2/W1TsH4hjx3VuLF87F6eSc+x0BgUgKqSgCjInol6lPThW2HoVgnFOGsnTDhut4w+j+ubZarNU3SPKv2+V1dXQV+MlAwLrj3pS/vB9HwnpqHJINPF2XXASQMV4YgIK7jQupGjgsgrHttH9t0mbUT0ELlI44cbX2UCH1bn1lziBUeiGvoEnzUWLfv6IrzNzKnHsXLSB92CJ8KTFqolHzLMcGNVIAFNKCD3aa7kqowbmhDSRvzlOYqAmkflcSJMR31s6SvrlUenj/3Ux/t2dpnsJAVbIXliS3rTzcl47sv4GV0ST4zBcrkqzIUVDC7zDyB2u1PqEm7TwF4A1CYYLwZ3DDHPJRMKFRDIvHVMBRZJoHR8HT+/R2xiLx5OpqSJYXRIFUQyqYHMymVVIVQWmUskFEryOwBDG06PoBalKiSy6kOvI1HvpN+/Qdvc+3wEeTasEmEr1EvnCseLrrepmX4RDAI/hyHlGGMG/XuqUjgMus+LQFk3IcEWgQklcL1MXwNaq2rR7A3C17GnnXf+MnByaub1rRg7GLsLf5R97b0RO6VdjynfXPsI1mZu41tjoUQ6OE0wLp9tmKAs84AtyX7S0iiHyYb5Z05nXK7qtZrqEt+Mf37Hf+cDoq2GcHsBatf+UvnpcHhI3BDx9LCsGwauyJLc3gPBKDuMixqIfMA4k4MA2XRfzWekr+nFiGJNnLaM8vmNa3v+eVLmGTm475gvJSu3XmhsOFGZuwQArc2UcrLitwdeSw7IA3FCUkUJiQJG755SBqpZgQf7AalSnriDfKfZJPk8NkJx1Bzfd0zA7/73hGbBWic6XggMHsxX4z3/PRP63tvDh5wXbfFBLuJHdZtHDEV2I2A/5G3lna0/vip+vT7M30Ls/OHywcGY8yFLnR+cezUF05eunHwZjqzYDAzKgOZMYVprMFHubpkf1tD6rsrH5n7Nzn37sjOnTv5jzbjMZ2C/wdDOz71qcgOTwAAAABJRU5ErkJggg==',
    color = '#2d7baf'
  ),
  list(
    name='James',
    nickname = 'Garter',
    logo='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',
    color = '#000000'
  ),
  list(
    name='Jordan',
    nickname='Jmerc',
    logo='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',
    color = '#ea6495'
  ),
  list(
    name='Lester',
    nickname='Lester',
    logo='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',
    color = '#b7b3b7'
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
    color = '#2e5e08'
  ),
  list(
    name='Paul',
    nickname='Pmac',
    logo='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',
    color = '#500f0b'
  ),
  list(
    name='Simon',
    nickname='Chief',
    logo='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',
    color = '#224d83'
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
