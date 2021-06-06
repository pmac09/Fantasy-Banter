library(shinydashboard)
library(shiny)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')


ui <- dashboardPage(
    dashboardHeader(title = "Fantasy Banter"),
    
    dashboardSidebar(
        collapsed = TRUE,
        sidebarMenu(
            id = "sidebarTabs",
            menuItem("Match Day",   tabName = "tabMatchday",  icon = icon("users"))
        )
    ),
    
    dashboardBody(
        tabItems(        
            tabItem(tabName = "tabMatchday", #source('./ui/matchday_ui.R', local= TRUE)$value)
## MATCHDAY START #######            
div(
  style='max-width: 940px;
         margin-left: auto;
         margin-right: auto;
         background-color: green;',

  
  
    
    
    
)


)## MATCHDAY END #######  

        )   
    )
)


server <- function(input, output) {

    sc_auth <- get_sc_auth(cid, tkn)
    sc_settings <- get_sc_settings(sc_auth)
    sc_me <- get_sc_me(sc_auth)
    
    
    output$test <- renderText({
        
    })
    
}

shinyApp(ui = ui, server = server)
