shinyServer(function(input, output) {

  data <- download(projectURL= DATABASE_URL, fileName= "ASL_DASHBOARD")
  
  
  data$GAME_STATS <- list2df(data$GAME_STATS)
  data$PLAYER <- list2df(data$PLAYER) %>%
    mutate(PLAYER_ID = as.numeric(PLAYER_ID))
  
  createPlayerTable <- function(data){
    temp <- inner_join(data$GAME_STATS, data$PLAYER, by=c('PLAYER_ID'='PLAYER_ID'))
  }
  
  
  
  output$test <- renderTable({
    createPlayerTable(data)
  })
  
})
