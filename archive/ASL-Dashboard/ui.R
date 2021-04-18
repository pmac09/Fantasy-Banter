dashboardPagePlus(
  
  title = "ASL - Addicts Supercoach League",
  
  ## HEADER ----
  header = dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "ASL"), 
      img(src = "https://image.flaticon.com/icons/svg/204/204074.svg"))
  ),
  
  ## SIDEBAR ----
  collapse_sidebar = TRUE,
  sidebar = dashboardSidebar(
    
    ## Menu items
    sidebarMenu(
      menuItem("Home", tabName = "tabHome", icon = icon("home")),
      menuItem("Settings", tabName = "tabSettings", icon = icon("cog"),
               menuSubItem("Update Data", tabName="tabUpdate", icon=icon("upload"))
               )
    )
  ),
  
  ## BODY ----
  body = dashboardBody(
    
    ## Tab Items
    tabItems(
      tabItem(tabName= "tabHome", "home"),
      tabItem(tabName= "tabUpdate", source('./ui/uiUpdate.R', local=TRUE)$value)
    )
  )
)
