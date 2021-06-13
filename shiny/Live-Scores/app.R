library(shiny)
library(miniUI)

source('www/supercoach_functions.R')

ui <- miniPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  gadgetTitleBar(
    'ASL Round 13',
    left  = NULL,
    right = miniTitleBarButton("btnRefreshFF", icon("sync"), primary = FALSE)
  ),
  
  miniTabstripPanel(
    #miniTabPanel('Live Ladder'),
    miniTabPanel('Game 1',uiOutput('pgGame1')),
    miniTabPanel('Game 2',uiOutput('pgGame2')),
    miniTabPanel('Game 3',uiOutput('pgGame3')),
    miniTabPanel('Game 4',uiOutput('pgGame4'))
  )
)


server <- function(input, output) {
  
  
  rv <- reactiveValues(refreshFanfooty = 0,
                       liveFixture = NULL)
  
  settings_data <- get_settings_data(cid,tkn)
  sc_auth <- settings_data$sc_auth
  vSeason <- 2021
  vRound <- settings_data$next_round
  
  # Get sc fixture
  fixture_data <-get_fixture_data(cid, tkn, vRound)
  
  ## Get the live fixture
  rv$liveFixture <- get_afl_fixture_data(get_afl_fixture(sc_auth, vRound))
  
  ## On button press, compare status to existing status
  observeEvent(input$btnRefreshFF, {
    
    # Get the live fixture
    newFixture <- get_afl_fixture_data(get_afl_fixture(sc_auth, vRound))
    
    # Check status are identical
    if(all(rv$liveFixture$status == newFixture$status)){
      # Trigger Fanfooty Refresh
      rv$refreshFanfooty = rv$refreshFanfooty + 1
    } else {
      # Update live fixture to trigger complete refresh
      rv$liveFixture <- newFixture
    }
  })
  
  # Get player data from supercoach
  player_data_r <- reactive({
    
    # Create static version of reactiveVal
    afl_fixture <- rv$liveFixture
    
    # Get player data
    player_data <- get_player_data(cid, tkn, vRound)
    
    # Join the fixture
    player_data <- player_data %>% 
      left_join(afl_fixture[,c('team1_abbrev','game_num')], by=c('team_abbrev'='team1_abbrev')) %>%
      arrange(coach, game_num)
    
    return(player_data)
  })
  
  ## Get the fanfooty fixture
  ff_fixture <- get_ff_fixture_data(vSeason = vSeason, vRound = vRound)
  
  # filter for the live games
  ff_live_games_r <- reactive({
    
    # Create static version of reactiveVal
    afl_fixture <- rv$liveFixture
    
    # get Live games
    ff_live_games <- ff_fixture %>%
      left_join(afl_fixture, by=c('home_team'='team1')) %>%
      filter(status == 'now')
    
    return(ff_live_games)
  })
  
  # Get live scores from fanfooty
  ff_live_scores_r <- reactive({
    
    # Trigger refresh
    rv$refreshFanfooty
    
    # Create static version of reactiveVal
    ff_live_games <- ff_live_games_r()
    
    # container for loop
    ff_live_scores <- tibble()
    
    # loop through live games
    for (game_id in ff_live_games$game_id){
      
      # get specific fanfooty data
      ff_game_data <- get_ff_game_data(game_id) %>%
        select(feed_id, supercoach) %>%
        rename(live_points = supercoach) %>%
        mutate(live_projection = round(live_points * 3300/sum(live_points)))
      
      # merge to container
      ff_live_scores <- bind_rows(ff_live_scores,ff_game_data) 
    }
    
    return(ff_live_scores)
  })
  
  # Create live data tables
  live_data_r <- reactive({
    
    player_data <- player_data_r()
    ff_live_scores<- ff_live_scores_r()
    
    
    if(nrow(ff_live_scores)==0){
      #combine all the data
      live_data <- player_data %>%
        mutate(name = paste0(substr(first_name,1,1),'.',last_name)) %>%
        select(feed_id, name, team_abbrev, game_num, status,
               team, coach, picked, type, position,
               projected_points, points) %>%
        mutate(live_points = NA) %>%
        mutate(live_projection=NA)      
    } else {
      #combine all the data
      live_data <- player_data %>%
        left_join(ff_live_scores, by=c('feed_id')) %>%
        mutate(name = paste0(substr(first_name,1,1),'.',last_name)) %>%
        select(feed_id, name, team_abbrev, game_num, status,
               team, coach, picked, type, position,
               projected_points, points, live_points, live_projection)
    }
    
    return(live_data)
  })
  
  displayGame <- function(fixture_data, game_num, live_data){
    
    game_data <- fixture_data %>%
      filter(fixture == game_num)
    
    homeTeam <- live_data %>%
      filter(coach == game_data$coach[1]) %>%
      arrange(desc(type), picked) %>%
      mutate(row_num = row_number()) %>%
      filter(row_num <= 18) %>%
      mutate(points = ifelse(status=='post' & is.na(points), 0 , points)) %>%
      mutate(projected_points = ifelse(status=='post', points, projected_points)) %>%
      mutate(points = ifelse(status =='now', live_points, points)) %>%
      mutate(projected_points = ifelse(status=='now', live_projection, projected_points)) %>%
      mutate(points = ifelse(is.na(points),0,points)) %>%
      arrange(game_num, team, desc(projected_points)) %>%
      select(name, team_abbrev, position, projected_points, points)
    
    awayTeam <- live_data %>%
      filter(coach == game_data$opponent_coach[1]) %>%
      arrange(desc(type), picked) %>%
      mutate(row_num = row_number()) %>%
      filter(row_num <= 18) %>%
      mutate(points = ifelse(status=='post' & is.na(points), 0 , points)) %>%
      mutate(projected_points = ifelse(status=='post', points, projected_points)) %>%
      mutate(points = ifelse(status =='now', live_points, points)) %>%
      mutate(projected_points = ifelse(status=='now', live_projection, projected_points)) %>%
      mutate(points = ifelse(is.na(points),0,points)) %>%
      arrange(game_num, team, desc(projected_points)) %>%
      select(name, team_abbrev, position, projected_points, points)


    homeHeader <- teamHeader(game_data$coach[1], game_data$team[1], sum(homeTeam$points), sum(homeTeam$projected_points))
    awayHeader <- teamHeader(game_data$opponent_coach[1], game_data$opponent_team[1],sum(awayTeam$points), sum(awayTeam$projected_points))
    
    homePlayers <- playerList(homeTeam, FALSE)
    awayPlayers <- playerList(awayTeam, TRUE)
    
    ui <- div(class='scores-container',
      div(class='scores-header',
        homeHeader,
        div(class='vs-header', div(class='vs-logo',"vs")),
        awayHeader
      ),
      div(class='scores-body',
        div(class='scores-team', homePlayers),
        div(class='scores-team', awayPlayers)
      )
    )
    
    return(ui)
  
  }
  teamHeader <- function(coachName, teamName, liveScore, liveProj){
    
    ui <- div(class='team-header',
              div(class='team-logo', img(class='sc-logo',src=paste0(coachName,'.png'))),
              div(class='team-name', teamName),
              div(class='team-coach', coachName),
              div(class='team-score', liveScore),
              div(class='team-proj', liveProj)
    )
    
    return(ui)
    
  }
  playerList <- function(team, alignRight){
    
    playerList <- list()
    for(i in 1:nrow(team)){
      
      playerList[[i]] <- playerCell(team$name[i], 
                                     team$team_abbrev[i], 
                                     team$position[i], 
                                     team$points[i], 
                                     team$projected_points[i], 
                                     alignRight)
    }
    
    return(playerList)
  }
  playerCell <- function(playerName, playerTeam, playerPos, liveScore, projScore, alignRight=FALSE){
    
    pLogo <- div(class='player-logo', img(class='afl-logo',src=paste0('colours/',playerTeam,'.png')))
    
    pName <- div(class=paste0(ifelse(alignRight, 'player-detail-right', 'player-detail')),
                 div(class='player-name', playerName), 
                 div(class='player-pos', playerPos))
    
    pScore <- div(class='player-score', 
                  div(class='player-live', liveScore), 
                  div(class='player-proj', projScore))
    
    
    if(alignRight) {
      divs <- list(pScore,pName,pLogo)
    } else {
      divs <- list(pLogo,pName,pScore)
    }
    
    cell <- div(class='player-cell', divs)
    
    return(cell)
  }
  
  # UI Ouptut
  output$pgGame1 <- renderUI({
    req(live_data_r())
    live_data <- live_data_r()
    
    ui <- displayGame(fixture_data, 1, live_data)
    return(ui)
  })
  output$pgGame2 <- renderUI({
    req(live_data_r())
    ui <- displayGame(fixture_data, 2, live_data_r())
    return(ui)
  })
  output$pgGame3 <- renderUI({
    req(live_data_r())
    ui <- displayGame(fixture_data, 3, live_data_r())
    return(ui)
  })
  output$pgGame4 <- renderUI({
    req(live_data_r())
    ui <- displayGame(fixture_data, 4, live_data_r())
    return(ui)
  })
    
  
  ## OBSERVES #####
  observeEvent(input$btnRefreshFF, {
    rv$refreshFanfooty <- rv$refreshFanfooty + 1
    print(rv$refreshFanfooty)
  })
  
}

shinyApp(ui = ui, server = server)
