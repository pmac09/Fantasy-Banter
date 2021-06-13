################################################################################
## SUPERCOACH FUNCTIONS

## Functions for interacting with the supercoach website and extracting data.

options(stringsAsFactors = FALSE)
library(httr)
library(tidyverse)
library(lubridate)
library(tictoc)
library(data.table)

source('www/secrets.R') # import supercoach authentication variables

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
get_afl_fixture <- function(sc_auth, round){
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
    status           = raw$played_status.status,
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
get_afl_fixture_data <- function(afl_fixture){
  
  raw <- lapply(afl_fixture, unlist)
  raw <- bind_rows(lapply(raw, as.data.frame.list))
  
  home <- tibble(
    season       = as.numeric(raw$season),
    round        = as.numeric(raw$round),
    game_num     = as.numeric(row.names(raw)),
    kickoff      = with_tz(as_datetime(raw$kickoff), "Australia/Melbourne"),
    status       = raw$status, 
    team1        = raw$team1.name,
    team1_abbrev = raw$team1.abbrev,
    team2        = raw$team2.name,
    team2_abbrev = raw$team2.abbrev,
    home_flag    = 1
  )
  
  away <- home %>%
    rename(temp = team1) %>%
    rename(team1 = team2) %>%
    rename(team2 = temp) %>%
    rename(temp_abbrev = team1_abbrev) %>%
    rename(team1_abbrev = team2_abbrev) %>%
    rename(team2_abbrev = temp_abbrev) %>%
    mutate(home_flag = 0)
 
  afl_fixture_data <- bind_rows(home,away) %>%
    arrange(kickoff, desc(home_flag))
  
  return(afl_fixture_data)
}

get_ff_fixture_data <- function(vSeason=NA, vRound=NA){
  
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
  
  return(ff_fixture)
}
get_ff_game_data <- function(game_id){
  
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
    'metres_gained'
  )
  
  game_data$feed_id <- as.numeric(game_data$feed_id)
  game_data$supercoach <- as.numeric(game_data$supercoach)
  
  return(game_data)
  
}

## Data Processing Scripts
get_settings_data <- function(cid, tkn){
  
  # Output Variable
  settings <- list()
  
  # Get Authentication
  sc_auth <- get_sc_auth(cid, tkn)
  settings$sc_auth <- sc_auth
  
  # Get settings for Round
  sc_settings <- get_sc_settings(sc_auth)
  settings$current_round <- sc_settings$competition$current_round
  settings$next_round <- sc_settings$competition$next_round
  
  # Get me for user ID
  sc_me <- get_sc_me(sc_auth)
  user_id <- sc_me$id
  settings$user_id <- user_id
  
  # Get user for league ID
  sc_user <- get_sc_user(sc_auth, user_id)
  league_id <- sc_user$draft[[1]]$leagues[[1]]$id
  settings$league_id <- league_id
  
  return(settings)
}
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
  
  fixture_data$team_score[fixture_data$team_score == 0] <- NA
  fixture_data$opponent_score[fixture_data$opponent_score == 0] <- NA
  
  fixture_data <- fixture_data %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0)) %>%
    group_by(coach) %>%
    mutate(cumul_win = cumsum(win)) %>%
    mutate(cumul_draw = cumsum(draw)) %>%
    mutate(cumul_loss = cumsum(loss)) %>%
    mutate(cumul_points_for = cumsum(team_score)) %>%
    mutate(cumul_points_against = cumsum(opponent_score)) %>%
    mutate(points = cumul_win*4 + cumul_draw*2) %>%
    mutate(pcnt = round(cumul_points_for/cumul_points_against*100,2))
  
  toc()
  return(fixture_data)
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


## Other Handy Variables
team_colours <- list(
  list(
    name = 'Anthony',
    nickname = 'Melons',
    logo='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',
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
    logo='iVBORw0KGgoAAAANSUhEUgAAACYAAAAmCAYAAACoPemuAAAIoklEQVRYR5WYC3QU1RnH/3dnN5sGax6tSaUJFBUCPRD0SBvQmhZbNG2g7akoUAtWEYseTq3YACZFCCgPj1XB8CgNr1aJFKSpp5UWCrY8ErBWCJY0BU+TGkJCAoQKabKPmdvz3Zlv9u5kF+LumXPv3Nmd+e3/++7/fneFlFLiE77aL7Vh9/G3UXvqMNovteP8xx04999zEBDISc/BDdfnqLZw6Fh867YS3Jg58BM+ARD9BQtFQ9j0lyq8eWQHjjW/ryCEEIDdU329hYQzBowaVIBJY76DmV+fhaA/2C/Ia4JZ0sL22mo8X7MErRda1cN8wgfD54chDPh9BgyfAR98CoxA6UXfk9JC1IpCBUVCKfeTkrmYVvSgusfVXlcFa7nQggcrp6C++bi6ER1+IwC/z+/A2TA0LiSpFjtXikmhgExpImSGYElTnQ/PHYGqJ7ZgcPbgpGxJwepO1+IHldNw/nKnUsNv+JVKtlqkkAPktJCxcxVSgiL9nJaum1YUPVYPLMtC5oAsrJ29Hnd9sSghXEKwHUd+g9lVj6owMJRPGI5ifggJF5DDp5RzwqnGSDGnJSg6p4PC2mv2IGyFETACWD7jBUwr+n4fuD5gpNTElcWImBGVQ0odyiPuU0gJwkdtLK90MIagMWnZk0CNWYC0JKRlISzDCMmQSoutT72Gr40aHwcXB0Y59dWKO+3wUT5R6AiMwuco5nOS31VHyyulkGUr48xRWJa0ldOgCI4+E0IYYRlCxoBM/H7Rbtz0uZtcOBeMZtH4JXcpK3igcAo2PLbJ/RDlhJppFs00qQ7qJzpM01Tj0WhUtXROB51zS30+NhxYh70n9yA/Nx/7lv9VRcjOTcdgqw+/jh9VPYoh2UNwuOIo0lIGqA/oIOzF/HBq+ToD6Ne8YAyjA3b3dqO8Zj5aL7Zi2Q9XYGbxrBgYmeftzxTgzIUz2P7kTtxTUOw+kJXyqsUAPK6rpKtF45FIxFWO+7pqJ8/+A8t3P4fs9GwcXf13pAXTbMXW7q1EWfV83JF/J/4w708uFIeMlWMFEoVSV0pXL5FK3pAS7Cvv/BwnWuoxb8oClE6eb4PdvbRI5ZZtos5MdBPe9ic2UUpr1ee3YwVsDWTyMZsALNM2VWnSbHTyk/uWhGVaajyKKMJGCMPzRuDQK3UQbV1n5Yinh6ovk4nGg9lADKKgNK9y/coxUWUNrneRNdgP5VlIEJZp1wx231LAqiV/C/RCwsJ7a45BVO3fIEtfm+uuf66zs185YK5StPQ474R+5dgF+5VSy4Ej6yAItUxFTXecrtN5xAgjaph4dvoiiEfWPyR/++6bWPPwejwwbqprBXpesS3Qr/LmEuddohmo24Se9NTXk5/O6TjRehxVdb/EhDETIEpWFMsjp2uxt/wdjMorcMG8NqEDeGG8vsUP1pNc7/N1vaV++8dtWLlvOQqGjIb4Utltsqnj32h48RQy0jL7zEi6Ib10e/CaJ/0IHtPVYGAdVL8eDofVfaml8e5QNxb98WcYmHkjxOA5A2V37//wn1db1RLESulh0/t6yLxhTeburBaHjM51tQiMvtsb7sXC3WVIDQQhvjAnV5L7Nq1uUWC6OuxXDMb+5A0lA3lDlyyvGNAbSgJbvGchUv2pEIXlt8umjiacWHES6WkZcTmmh0/PI+5710U9ofU+h8urFIO5oey9guf3LcXnPzsQYtLKEnn0wzrl+CNzR7oLr77UJHJ1Vo1zS3d4zh0dhK8nUovBOi6fQ2Xtaoy+eTTErF/MlG+9V4OXp6/Cd8d8Ly7J2Sa8IdTPr5ZXyWwhWSj/1dmIN+q34d7CeyE27q+StE6mGAEEfCmuy7tFoFN5xi05zjJkO7qz1GgmyoZKrevwjonyuWlaylTpnFuZZkEEBRbPrIA4e7FVjnnmVlD5mxZIi62BzvJDN3eXGVWRSlXHe6HIzdVDnSKQ10AG0VuGcsGofKL7ZkgIQ+CDLQ32Iv7NZRNQ31yPT/lT4fcF4hZoBlPbYmeBZqi4xZnUYdWchTkZlK6Squnoe4YFcb3A8MHDcXzTBzbYuj1r8NzOCnz55rHYOGuLW5kmqquSFX/JZmQoFFJ5652BuqfRd3c17sTp86dQ9lA5np2x2AajQvEr5YVo62rDuoc3YOwtd7hOrs86dnhOeG8B6IVj46RxPeGpr8/cs1dasfX9zcjOuAGN2z6MFYpkqtUHX0fpr+ciL2sQtj2xXU0Er8vzbEw2E3VL0GGoz5AMyPcIR0PYWr8ZnVc6sXL2C3jy/qfia37ajBQv/Qb+2dKAklsnoWzSwoRgdGM2WF25RNUCK0pQOpj+w/Y3/xl/O/Muhg0ahmMbT/TdjBDmR50fYeKyYnRdvogUXxApIuhuVO26yoptw2inFFeVWrCiTvFH15y+STuliL1zcm1BTRQTSAVEmkD6dek4UHkY+Xn5fbdvPHKo8RCmvzQVETOKoIjBKSit6KNqVa0OXB5TJUpbOqcyJTCC0meg62n0h0tQKiiqmrdX7EDJuInJN7x85Y2D1Vjwq1JEohGkiBQFyBtZBZLAFnQzZYCo10Cd/SjSJJBil/IvznkJs7/9+LX/IuBPHGw4gMfXPoauK10QlkBQpMIvjT5QDKSXytTXnd02Xkt5lUy1AMMO36YFW/ooxc+/6t9QzR3NeGTVDDS2NKqQGdJAQAYgLJ+764mFR6pcUjseRykVRtqZCxMySGD2YynRdyzZFZdTXsn69cfd1r2bseqtl9F+sd1eiuhHSz8EFbeWgIxKd7ejLMUy1T9FJkxYhkn/qqjnkk89PbUUc+77sTv7+sTQGbgmGH+xJ9yDdW+vRc3BXWg806j2i3q+qaKSlyLetkmpmIbl5eO+8ZPx0ynzlHn259VvMP1mze1N+F1dDeoaatF+4Rw6uzrQealDgWZel4WsT38GOVk5GDdyHCYX3Y9bcof2hyXuM/8HvINA4RrwvWYAAAAASUVORK5CYII=',
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
