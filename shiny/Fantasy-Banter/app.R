library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

library(DT)

#func_path <- '/Users/paulmcgrath/Github/Fantasy-Banter/functions/'
#func_path <- './shiny/Fantasy-Banter/www/'
func_path <- './www/'
source(paste0(func_path, 'supercoach_functions_V2.R')) # import authentication variables

#source('./www/styles.R', local=TRUE)
source('./www/styles.R', local=TRUE)

ui <- dashboardPage(
  title = "Fantasy Banter", 
  
  ## HEADER ----
  dashboardHeader(title = 'FB'),
  
  ## SIDEBAR ----
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Trade Machine", tabName = "tabTrade", icon = icon("rotate")),
      menuItem("Player Rankings", tabName = "tabPlayers", icon = icon("users"))
    )
  ),
  
  ## BODY ----
  dashboardBody(
    tags$head(tags$style(styles)),
    
    tabItems(
      tabItem(
        tabName = "tabTrade",
        fluidRow(h1(style='text-align:center;', "ASL TRADE MACHINE")),
        fluidRow(
          column(width=6,
                 uiOutput('uiSelectTeamLeft'),
                 uiOutput('uiSelectPlayersLeft'),
                 uiOutput('uiTeamSummaryLeft'),
                 uiOutput('uiTeamDisplayLeft')),
          column(width=6, 
                 uiOutput('uiSelectTeamRight'),
                 uiOutput('uiSelectPlayersRight'),
                 uiOutput('uiTeamSummaryRight'),
                 uiOutput('uiTeamDisplayRight')),
        )
      ),
      tabItem(
        tabName = "tabPlayers",
        'Players'
        #source('./ui/uiSettings.R', local= TRUE)$value
      )
    )
  )
)

## SERVER -----
server <- function(input, output, session) {
  
  sc <- sc_setup(cid, tkn)
  
  source('./server/serverTrades.R', local= TRUE)$value
  
}

## RUN -----
shinyApp(ui = ui, server = server)

