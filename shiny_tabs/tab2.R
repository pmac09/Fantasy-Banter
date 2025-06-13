# Tab 2 UI (no namespacing)
tab2UI <- function() {
  f7Tab(
    tabName = "tab2",
    icon = f7Icon("info_circle"),
    f7Card(
      title = "About This App",
      "This is a simple ShinyMobile app with two tabs and no namespacing."
    )
  )
}

# Tab 2 Server
tab2Server <- function(input, output, session) {
  # No dynamic content yet
}
