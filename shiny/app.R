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
            tabItem(tabName = "tabMatchday", source('./ui/matchday_ui.R', local= TRUE)$value)
        )   
    )
)


server <- function(input, output) {

    sc_auth <- get_sc_auth(cid, tkn)
    
    print(sc_auth)
    
    output$test <- renderText({
        
    })
    
}

shinyApp(ui = ui, server = server)
