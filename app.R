library(shiny)
library(shinyMobile)
library(reactable)
library(htmltools)

source('R/fantasy-banter-functions.R')
source("shiny_tabs/tabDash.R")
source("shiny_tabs/tabTrade.R")
source("shiny_tabs/tabSettings.R")

app_options <- list(
  theme = "ios",
  dark = TRUE,
  filled = FALSE,
  preloader = TRUE,
  color = "red",
  navbar = list(
    hideOnPageScroll = TRUE,
    mdCenterTitle = TRUE
  )
)

## App -------------------------------------------------------------------------

shinyApp(

  ui = f7Page(
    title = "Fantasy Banter",
    options = app_options,
    

    f7TabLayout(
      navbar = f7Navbar(
        title = "Fantasy Banter",
        hairline = TRUE,
        leftPanel = FALSE,
        rightPanel = FALSE
      ),
      f7Tabs(
        animated = FALSE,
        swipeable = FALSE,
        tabDashUI(),
        tabTradeUI(),
        tabSettingsUI()
      )
    )
  ),

  server = function(input, output, session) {
    
    ## Enable auto-refresh on app load
    scKey <- sc_key(databaseURL)  
    scAuth <- sc_authenticate(scKey$client_id, scKey$access_token)
    sc <- sc_setup(scAuth)
    
    ## Base data

    fb <- reactiveValues(
      league = fb_league(),
      players = fb_players()
    )
    
    # Tab Settings
    tabDashServer(input, output, session, sc, fb)
    tabTradeServer(input, output, session, sc, fb)
    tabSettingsServer(input, output, session, sc, fb)
  }
)
