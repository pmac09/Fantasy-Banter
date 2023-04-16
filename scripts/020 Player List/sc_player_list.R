################################################################################
# SETUP

options(stringsAsFactors = FALSE)
library(tidyverse)
library(data.table)
library(httr)

################################################################################
# FUNCTIONS
get_sc_auth <- function(cid, tkn){
  
  year <- year(Sys.Date())
  url <- paste0('https://supercoach.heraldsun.com.au/',year,'/api/afl/classic/v1/access_token')
  
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
  
  headers <- add_headers(
    Authorization = paste0('Bearer ', auth$access_token)
  )
  
  return(headers)
}       # Generate SC auth access token
get_data <- function(auth_headers, url){
  data <- content(GET(
    url = url,
    config = auth_headers,
    referrer = "https://supercoach.heraldsun.com.au/afl/draft/predraft"
  ))
  return(data)
} # Get SC data

################################################################################
# SCRIPT

# Get supercoach auth variables
source('./functions/secrets.R')

# Generate auth_token
auth_headers <- get_sc_auth(cid, tkn)

# Get current year player list
year <- year(Sys.Date())

# Build HTTP url
url <- paste0('https://supercoach.heraldsun.com.au/',year,'/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&round=0')

# Get player data
players_raw <- get_data(auth_headers, url)
players_raw <- lapply(players_raw, unlist)
players_raw <- bind_rows(lapply(players_raw, as.data.frame.list))

# Format player data
player_data <- tibble(
  player_id        = as.numeric(players_raw$feed_id),
  supercoach_id    = as.numeric(players_raw$id),
  first_name       = players_raw$first_name,
  last_name        = players_raw$last_name,
  team             = players_raw$team.abbrev,
  pos              = trimws(paste(players_raw$positions.position, ifelse(is.na(players_raw$positions.position.1),"",players_raw$positions.position.1))),
  average          = as.numeric(players_raw$previous_average),
  price            = as.numeric(players_raw$player_stats.price)
) %>%
  arrange(desc(price))

# Export player list
path <- paste0('./scripts/02 PLayer List/sc_player_list_',year,'.csv')
write.csv(player_data, path, row.names = FALSE, na="")
