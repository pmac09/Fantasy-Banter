# Tab 2 UI (no namespacing)
tabTradeUI <- function() {
  f7Tab(
    tabName = "tabTrade",
    icon = f7Icon("repeat"),
    f7Card(
      title = "About This App",
      "This is a simple ShinyMobile app with two tabs and no namespacing."
    ),
    f7Segment(
      shadow = TRUE,
      f7Button(label = "My button", outline = TRUE, fill = FALSE, shadow = TRUE),
      f7Button(label = "My button", outline = TRUE, fill = FALSE, shadow = TRUE),
      f7Button(label = "My button", outline = TRUE, fill = FALSE, shadow = TRUE)
    )
  )
}

# Tab 2 Server
tabTradeServer <- function(input, output, session, sc, fb) {
  # No dynamic content yet
}
