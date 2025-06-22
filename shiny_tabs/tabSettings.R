tabSettingsUI <- function() {
  f7Tab(
    tabName = "tabSettings",
    icon = f7Icon("info_circle"),
    f7Card(
      title = "About This App",
      "This is a simple ShinyMobile app with two tabs and no namespacing."
    ),
    f7Segment(
      shadow = TRUE,
      f7Button("btnRefreshSC", label = "Refresh", outline = TRUE, fill = FALSE, shadow = TRUE)
    )
  )
}

tabSettingsServer <- function(input, output, session, sc, fb) {
  
  observeEvent(input$btnRefreshSC, {sc_autoUpdate(sc)})
  
  
}
