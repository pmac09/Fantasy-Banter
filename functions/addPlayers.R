library(tidyverse)
library(fireData)

addPlayers <- function(gameNo){
  
  url <- paste0("http://live.fanfooty.com.au/chat/", gameNo, ".txt")
  data <- tryCatch({strsplit(readLines(url),",")},
                   error=function(cond)   {NULL},
                   warning=function(cond) {NULL})
  
  if(is.null(data)){stop('No data found.')}
  if(length(data[[3]]) == 0) {stop('Game has not started.')}
  
  
  # Download player list from firebase
  firebasePlayers <- download(DATABASE_URL, fileName = "ASL_DASHBOARD/PLAYER")
  
  # Create game variables
  vSeason <- as.numeric(data[[2]][2])                               # SEASON
  
  # Clean data for upload to firebase
  players <- list()
  for(i in 5:length(data)){
    
    vPlayerId <- as.numeric(data[[i]][1])                           # PLAYER ID
    vId <- paste0(vPlayerId)                                        # RECORD ID
    
    # Check if player is already in firebase
    if(!(is.null(firebasePlayers[[vId]]))){
      next
    }
    
    vFirstName <- data[[i]][2]                                      # FIRST NAME
    vLastName  <- data[[i]][3]                                      # LAST NAME
    
    players[[vId]]$PLAYER_ID  <- vPlayerId
    players[[vId]]$FIRST_NAME <- vFirstName
    players[[vId]]$LAST_NAME  <- vLastName
    
  }
  
  patch(players, DATABASE_URL, directory = "ASL_DASHBOARD/PLAYER")
  
  msg <- paste0(length(players), ' players added to firebase')
  
  return(msg)  
  
}

addPlayers(7028)

