shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput('txtToken')
    )
  )
))
