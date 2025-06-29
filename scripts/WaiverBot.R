library(httr)
library(jsonlite) 

# Load supercoach functions
source('./R/fantasy-banter-functions.R')


# WAIVER BOT FUNCTIONS ---------------------------------------------------------

nextWaiver <- function(sc, playerStatusURL){
  
  data <- bind_rows(sc_download(sc$auth, playerStatusURL))
  
  if(has_name(data,'waiver_until')){
    waiverStatus <-  suppressMessages(format(as_datetime(min(data$waiver_until, na.rm=T), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S"))
  } else {
    waiverStatus <- NA
  }
  
  return(waiverStatus)
}
printPlayerTable <- function(ids){
  msg <- players %>%
    filter(playerID %in% ids) %>%
    select(playerID, playerName, teamAbbrev, pos) %>%
    arrange(match(playerID,ids))
  msg <- paste(capture.output(print(as.data.frame(msg))), collapse = "\n")
  message(msg)
}


# RUN WAIVER BOT ---------------------------------------------------------------

print_log("WAIVER BOT")

## Get player data 
players <- sc_players(sc,sc$var$current_round)

## Player IDs to Drop and Add
drop_ids <- c(222)
add_ids <- c(208,71,366)

## Print Players to DROPs
print_log('Players to DROP:')
printPlayerTable(drop_ids)

## Print Players to ADD
print_log('Players to ADD:')
printPlayerTable(add_ids)

## Confirm FA picks
response <- readline(prompt = "Confirm free agency trades? (Y/N): ")
if (tolower(response) != "y") {
  print_log('Cancelled.')
  stop()
}
print_log("Confirmed")

# determine next waiver time
playerStatusURL <- sc$url$playerStatus
wvr <- nextWaiver(sc, playerStatusURL)

if(is.na(wvr)) {
  fa_open <- TRUE
  print_log('Free Agency Open')
} else {
  wait <- round(as.numeric(difftime(wvr, Sys.time(), units=c('secs'))),0)
  print_log(paste0('Sleeping until the waiver: ', wvr))
  if(wait >0) Sys.sleep(wait)
  print_log(paste0('Start Free Agency monitor '))
  fa_open <- FALSE
}

# loop until fa open
while(!fa_open){

  # wait to avoid DDOS
  Sys.sleep(30)

  # download player status data
  checkWvr <- tryCatch(nextWaiver(sc, playerStatusURL),
    error = function(cond) {
      print_log(paste0('ERROR'))
      NULL
    })

  if(is.null(checkWvr)) next

  if(wvr != checkWvr){
    fa_open <- TRUE
    print_log('Free Agency Open')
  } else {
    print_log(paste0('Free Agency Closed', checkWvr))
  }

}


# loop until no more add or drops
while(length(drop_ids) > 0 & length(add_ids) > 0){

  # Get current team
  teamURL <- paste0("https://www.supercoach.com.au/2025/api/afl/draft/v1/userteams/",sc$api$user$classic$id,"/players")
  current_team <- sc_download(sc$auth, teamURL)
  current_team <- lapply(current_team, function(x) x[c('player_id','picked','position')])

  # validate DROPS
  drop_ids <- drop_ids[drop_ids %in% sapply(current_team, function(t) t$player_id)]
  print_log(paste0('Drops Remaining: ',length(drop_ids)))
  
  # Exit loop if no one left to drop
  if(length(drop_ids)==0) next

  # Create DROP request
  drop_player <- drop_ids[1]
  drop_team <- Filter(function(player) player$player_id != drop_player, current_team)

  # Get Free Agents via Player Status
  playerStatus <- sc_download(sc$auth, playerStatusURL)
  free_agents <- sapply(playerStatus, function(t) ifelse(t$trade_status=='free_agent', t$player_id, NA))
  free_agents <- free_agents[!is.na(free_agents)]

  # validate ADDS
  add_ids <- add_ids[add_ids %in% free_agents]
  print_log(paste0('Adds Remaining: ',length(add_ids)))

  # Exit loop if no one left to drop
  if(length(add_ids)==0) next

  # Create ADD request
  add_player <- add_ids[1]
  add_team <- append(drop_team, list(list(player_id = add_player,
                                     position = unname(players$pos1[players$playerID==add_player]),
                                     picked = 'false')))

  # Check team has right number of players
  if(length(current_team) != length(add_team)) next

  # Create API body
  body <- list(
    live_draft_mode = FALSE,
    current = current_team,
    selections = add_team
  )
  
  fa_trade <- paste0("Dropping ", 
                     players$playerName[players$playerID==drop_player],
                     ' for ',
                     players$playerName[players$playerID==add_player])
  print_log(fa_trade)
  
  # Send the PUT request
  response <- PUT(teamURL, sc$auth, body = body, encode = "json")
  
  if(response$status_code=='200'){
    print_log('SUCCESS')
  } else {
    print_log('FAILED')
    parsed_response <- fromJSON(cont(response, "text"))
    print(parsed_response)
    stop()
  }
  
}

print_log('WAIVER BOT - COMPLETE')
