shinyServer(function(input, output) {
  
  ## Get Supercoach access token using python function
  token <- getToken(SC_CID, SC_USR, SC_PWD)
  
  ## Scrape Supercoach stats centre
  stats.url <- paste0('https://supercoach.heraldsun.com.au/afl/draft/statscentre?access_token=', token)
  stats.html <- strsplit(readLines(stats.url), "\n")
  stats.string <- stats.html[[grep("var researchGridData",stats.html)]]
  stats.data <- fromJSON(substr(stats.string,24,nchar(stats.string)))
  
  
  
  
  
  data <- download(projectURL= DATABASE_URL, fileName= "ASL_DASHBOARD")
  
  
  data$GAME_STATS <- list2df(data$GAME_STATS)
  data$PLAYER <- list2df(data$PLAYER) %>%
    mutate(PLAYER_ID = as.numeric(PLAYER_ID))
  
  createPlayerTable <- function(data){
    temp <- inner_join(data$GAME_STATS, data$PLAYER, by=c('PLAYER_ID'='PLAYER_ID'))
  }
  
  
  output$test <- renderTable({
    ##createPlayerTable(data)
    as.data.frame(unlist(lapply(stats.data, function(x) x$ln)))
  })
  
})
