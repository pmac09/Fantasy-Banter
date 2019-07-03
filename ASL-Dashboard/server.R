shinyServer(function(input, output) {

  data <- download(projectURL= DATABASE_URL, fileName= "ASL_DASHBOARD")
  
  output$test <- renderTable({
    data$SC_COACH
  })
  
})
