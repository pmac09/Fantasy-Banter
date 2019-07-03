dashboardPagePlus(
  
  title = "ASL - Addicts Supercoach League",
  
  ## HEADER ----
  header = dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "ASL"), 
      img(src = "https://image.flaticon.com/icons/svg/204/204074.svg"))
  ),

  ## SIDEBAR ----
  sidebar = dashboardSidebar(
    
  ),
  
  ## BODY ----
  body = dashboardBody(
    
    tableOutput("test")
    
  )

)
