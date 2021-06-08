library(shiny)
library(html2canvas)

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
            uiOutput('uiLogo')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')
    
    settings <- get_settings_data(cid,tkn)
    
    sc_league <- get_sc_league(settings$sc_auth, settings$league_id)
    
    raw <- sc_league$ladder
    raw <- lapply(raw, unlist)
    raw <- bind_rows(lapply(raw, as.data.frame.list))
    
    teams<- raw[,grepl('userTeam.',colnames(raw))]
    colnames(teams) <- sub('userTeam.', '', colnames(teams))
    teams <- as_tibble(teams[,c(1:12)])
    
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

}

# Run the application 
shinyApp(ui = ui, server = server)
