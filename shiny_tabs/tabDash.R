# UI function (no namespacing)
tabDashUI <- function() {
  f7Tab(
    tabName = "tabDash",
    icon = f7Icon("speedometer"),
    active  = TRUE,      
    f7Card(title = "Standings",reactableOutput('fbStandings', height = "auto"))
    #f7Card(title = "Power Rankings",reactableOutput('fbStandings', height = "auto"))
  )
}

# Server function (no moduleServer)
tabDashServer <- function(input, output, session, fb) {

  output$fbStandings <- renderReactable({fb_standings(fb$league)})

}
