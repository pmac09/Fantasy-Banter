library(fireData)

addPlayerXteam <- function(gameNo){
  
  url <- paste0("http://live.fanfooty.com.au/chat/", gameNo, ".txt")
  data <- tryCatch({strsplit(readLines(url),",")},
                   error=function(cond)   {NULL},
                   warning=function(cond) {NULL})
  
  if(is.null(data)){stop('No data found.')}
  if(length(data[[3]]) == 0) {stop('Game has not started.')}
  
  
  # Download data from firebase
  firebaseData <- download(DATABASE_URL, fileName = "ASL_DASHBOARD")
  
  # Create game variables
  vSeason <- as.numeric(data[[2]][2])                               # SEASON
  
  # Clean data for upload to firebase
  pxt <- list()
  for(i in 5:length(data)){
    
    vPlayerId <- as.numeric(data[[i]][1])                           # PLAYER ID
    vId      <- paste0(vSeason,'-',vPlayerId)                       # RECORD ID
    
    # Check if player is already in firebase
    if(!(is.null(firebaseData$PLAYER_X_TEAM[[vId]]))){
      next
    }
    
    vJersey  <- as.numeric(data[[i]][30])                           # JERSEY NO
    vTeamFF  <- data[[i]][4]                                        # TEAM FANFOOTY
                                                                    # TEAM ID
    vTeamId <- firebaseData$TEAM$TEAM_ID[firebaseData$TEAM$TEAM_FANFOOTY == vTeamFF & !is.na(firebaseData$TEAM$TEAM_FANFOOTY)]
    
    pxt[[vId]]$SEASON     <- vSeason
    pxt[[vId]]$PLAYER_ID  <- vPlayerId
    pxt[[vId]]$TEAM_ID    <- vTeamId 
    pxt[[vId]]$JERSEY     <- vJersey
    
  }
  
  patch(pxt, DATABASE_URL, directory = "ASL_DASHBOARD/PLAYER_X_TEAM")
  
  msg <- paste0(length(pxt), ' players added to firebase')
  
  return(msg)  
  
}




