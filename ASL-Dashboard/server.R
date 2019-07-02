shinyServer(function(input, output) {
   
  token <- getToken(SC_CID,SC_USR,SC_PWD)
  print(token)
  
  output$txtToken <- renderText(
    token
  )

})
