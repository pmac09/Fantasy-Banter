# UI function (no namespacing)
tab1UI <- function() {
  f7Tab(
    tabName = "tab1",
    icon = f7Icon("house"),
    f7Card(
      title = "Welcome",
      textInput("user_name", "Enter your name"),
      textOutput("greeting")
    )
  )
}

# Server function (no moduleServer)
tab1Server <- function(input, output, session) {
  output$greeting <- renderText({
    paste("Hello", input$user_name)
  })
}
