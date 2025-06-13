library(shiny)
library(shinyMobile)
library(reactable)

source('R/fantasy-banter-functions.R')
source("shiny_tabs/tab1.R")
source("shiny_tabs/tab2.R")

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
        swipeable = TRUE,
        tab1UI(),
        tab2UI()
      )
    )
  ),

  server = function(input, output, session) {
    tab1Server(input, output, session)
    tab2Server(input, output, session)
  }
)


# 
# shinyApp(
#   
#   ui = f7Page(
#     title = "Fantasy Banter",
#     options = app_options,
#     
#     f7TabLayout(
#       navbar = f7Navbar(
#         title = "Fantasy Banter",
#         hairline = TRUE,
#         leftPanel = FALSE,
#         rightPanel = FALSE
#       ),
#       
#       # Main Tabs
#       f7Tabs(
#         id = "tabset",
#         animated = FALSE,
#         swipeable = TRUE,
#         
#         f7Tab(title = "Summary", tabName = "Tab1", icon = f7Icon("chart_bar_square"), active = TRUE,
#           
#           f7Card(
#             title = "Ladder",
#             'Content',
#             footer = div(f7Toggle('btnAllTimeLadder', 'All Time', checked = FALSE, color = 'red'))
#           )
#               
#               
#         ),
#         
#         f7Tab(title = "Data", tabName = "Tab2", icon = f7Icon("doc_plaintext"),
#           f7Card(outline = TRUE, raised = TRUE, divider = TRUE, title = "League Data", reactableOutput("tblLeagueData")),
#           f7Card(outline = TRUE, raised = TRUE, divider = TRUE, title = "Player Data")
#         )
#     
#       )
#     )
#   ),
#   
#   
#   server = function(input, output, session) {
#     
#     league_data <- firebase_download(databaseURL, '/Fantasy-Banter/data/league', cols=TRUE)
#     
#     ladder_data <- league_data %>%
#       filter(!is.na(team_score)) %>%
#       filter(round == max(round)) %>%
#       arrange(position)
#     
#     rating_column <- function(maxWidth = 55, class = NULL, ...) {
#       colDef(maxWidth = maxWidth, align = "center", class = paste("cell number", class), ...)
#     }
#     
#     tbl <- reactable(
#       ladder_data[,c('coach','points','wins','draws','losses','points_for','points_against','pcnt')],
#       pagination = FALSE,
#       defaultColDef = colDef(class = "cell", headerClass = "header"),
#       columns = list(
#         coach = colDef(
#           defaultSortOrder = "asc",
#           minWidth = 125,
#           headerStyle = list(fontWeight = 700, position = "sticky", left = 0, background = "#fff", zIndex = 1), 
#           style= list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
#           class='cell border-right',
#           cell = function(value, index) {
#             div(
#               class = "team",
#               #img(class = "flag", alt = paste(value), src = sprintf("pages/images/%s_2022.png", value)),
#               div(class = "team-name", value)
#               #div(class = "record", sprintf("%s pts.", forecasts[index, "points"]))
#             )
#           }
#         ),
#         points = rating_column(name = "PTS", cell=function(value){format(floor(value), nsmall = 0)}),
#         wins = rating_column(name = "W - D - L",  maxWidth = 70,
#                              cell = function(value, index) {paste0(value,"-",ladder_data[index,'draws'],"-",ladder_data[index,'losses'])}),
#         draws = colDef(show = FALSE),
#         losses = colDef(show = FALSE),
#         points_for = rating_column(name = "Average Score",maxWidth = 70,class='small-font'),
#         points_against = rating_column(name = "Average Against",maxWidth = 70,class='small-font'),
#         pcnt = rating_column(name = "%",class='small-font', cell=function(value){format(round(value, 1), nsmall = 1)})
#       ),
#       
#       # Emphasize borders between groups when sorting by group
#       rowClass = JS("
#     function(rowInfo, state) {
#       const firstSorted = state.sorted[0]
#       if (firstSorted && firstSorted.id === 'group') {
#         const nextRow = state.pageRows[rowInfo.viewIndex + 1]
#         if (nextRow && rowInfo.row.group !== nextRow.group) {
#           return 'group-last'
#         }
#       }
#     }"
#       ),
#       showSortIcon = FALSE,
#       borderless = TRUE,
#       class = "standings-table"
#     )
#     
#     
#     
#     output$tblLeagueData <- renderReactable({tbl})
#     
# 
#   }
# )
