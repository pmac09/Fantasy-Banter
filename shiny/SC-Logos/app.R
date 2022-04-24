library(shiny)
library(html2canvas)
library(highcharter)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Download Supercoach Logo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput('uiTeamList'),
            numericInput("uiLogoSize", "Select Size:", 38),
            html2canvas("btnDownload", 
                        label = "Download Logo", 
                        selector = "#scLogo",
                        width='100%')
        ),
    
        mainPanel(
            uiOutput('uiLogo'),
            highchartOutput("test")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')
    
  sc <- get_sc(cid,tkn)  
  sc_league <- get_sc_data(sc$auth, sc$url$league[1])
  
  raw <- sc_league$ladder
  raw <- lapply(raw, unlist)
  raw <- bind_rows(lapply(raw, as.data.frame.list))
  
  teams<- raw[,grepl('userTeam.',colnames(raw))]
  colnames(teams) <- sub('userTeam.', '', colnames(teams))
  teams <- as_tibble(teams[,c(1:12)]) %>%
    mutate(shortcolor = ifelse(shortcolor=='7,0',basecolor,shortcolor))
  
  output$uiTeamList <- renderUI({
      selectInput("teamSelector",
                  "Select Team:",
                  choices=teams$teamname)
  })
  
  teamIndex <- reactive({
      req(input$teamSelector) 
      return(grep(input$teamSelector, teams$teamname)[1])
  })
  
  shirttype <- reactive({return(teams$shirttype[teamIndex()])})
  shortcolor <- reactive({return(teams$shortcolor[teamIndex()])})
  basecolor <- reactive({return(teams$basecolor[teamIndex()])})
  secondcolor <- reactive({return(teams$secondcolor[teamIndex()])})

  output$uiLogo <- renderUI({
      get_sc_logo(input$uiLogoSize, shirttype(), shortcolor(), basecolor(), secondcolor())
  })
  
  output$test <- renderHighchart({
      
      highchart() %>%
          hc_chart(type='line') %>%
          hc_title(text='Ladder Journey') %>%
          hc_xAxis(opposite=TRUE, title=list(text='Round'), tickInterval=1) %>%
          hc_yAxis(min=1,max=8, reversed=TRUE, categories=c(NA,1:8),
                   title=list(text='Ladder Position')) %>%
          hc_add_series(name="Test",
                        data=c(1:8),
                        marker=list(symbol='Paul.png'))
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
