library(fireData)

addGameStats <- function(gameNo){
  
  url <- paste0("http://live.fanfooty.com.au/chat/", gameNo, ".txt")
  data <- tryCatch({strsplit(readLines(url),",")},
                   error=function(cond)   {NULL},
                   warning=function(cond) {NULL})
  
  if(is.null(data)){stop('No data found.')}
  if(length(data[[3]]) == 0) {stop('Game has not started.')}
  
  # Create game variables
  vSeason <- as.numeric(data[[2]][2])                               # SEASON
  vRound <- as.numeric(substr(data[[1]][5],2,nchar(data[[1]][5])))  # ROUND
  
  players <- list()
  for(i in 5:length(data)){
    
    vPlayerId <- as.numeric(data[[i]][1])                           # PLAYER ID
    vId <- paste0(vSeason,"-",vRound,"-",vPlayerId)                 # RECORD ID
    
    vSupercoach <- as.numeric(data[[i]][7])                         # SUPERCOACH
    
    players[[vId]]$SEASON     <- vSeason
    players[[vId]]$ROUND      <- vRound
    players[[vId]]$PLAYER_ID  <- vPlayerId
    players[[vId]]$SUPERCOACH <- vSupercoach
    
  }

  patch(players, DATABASE_URL, directory = "ASL_DASHBOARD/GAME_STATS")
  
  msg <- paste0(length(players), ' player statlines added to firebase')
  
  return(msg)  
  
}
