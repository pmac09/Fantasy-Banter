shinyServer(function(input, output) {
  
  source('./server/svrUpdate.R', local=TRUE)$value
  
})
