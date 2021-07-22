## SETUP ----
library(shiny)
library(miniUI)
library(DT)

source('www/supercoach_functions.R')

## GLOBAL ----
g <- list(sc = get_sc(cid,tkn))

g$ladder_data <-get_sc_ladder_data(get_sc_league(g$sc$auth, 
                                                 g$sc$user$draft[[1]]$leagues[[1]]$id, 
                                                 g$sc$settings$competition$current_round))

g$fixture_data <- get_sc_fixture_data(get_sc_league(g$sc$auth, 
                                                    g$sc$user$draft[[1]]$leagues[[1]]$id, 
                                                    g$sc$settings$competition$next_round))

g$player_data <- get_player_data(g$sc, g$sc$settings$competition$next_round)

g$ff_fixture <- get_ff_fixture_data(vSeason = year(with_tz(as_datetime(Sys.time()), "Australia/Melbourne")), 
                                    vRound = g$sc$settings$competition$next_round)

g$afl_fixture <- get_afl_fixture_data(get_afl_fixture(g$sc$auth, 
                                                      g$sc$settings$competition$next_round))

## UI ----
ui <- miniPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  gadgetTitleBar(
    'ASL Live Scores',
    left  = NULL,
    right = miniTitleBarButton("btnRefreshFF", icon("sync"), primary = FALSE)
  ),
  
  miniTabstripPanel(
    miniTabPanel('Game 1',icon=icon('user-friends'),uiOutput('pgGame1')),
    miniTabPanel('Game 2',icon=icon('user-friends'),uiOutput('pgGame2')),
    miniTabPanel('Game 3',icon=icon('user-friends'),uiOutput('pgGame3')),
    miniTabPanel('Game 4',icon=icon('user-friends'),uiOutput('pgGame4')),
    miniTabPanel('Ladder', icon=icon('align-justify'), DTOutput('tbl'))
  )
)

## SERVER ----
server <- function(input, output, session) {

  # Initialise Reactive Variables
  rv <- reactiveValues(sc = g$sc,
                       player_data = g$player_data,
                       ladder_data = g$ladder_data,
                       fixture_data = g$fixture_data,
                       ff_fixture = g$ff_fixture,
                       afl_fixture = g$afl_fixture,
                       vRound = g$sc$settings$competition$next_round,
                       refreshFanfooty = 0,
                       refreshSupercoach = NULL)
  
  ## On button press, compare status to existing status
  observeEvent(input$btnRefreshFF, {
    
    # Get the live fixture
    newFixture <- get_afl_fixture_data(get_afl_fixture(rv$sc$auth, rv$vRound))
    
    # Check status are identical
    if(all(rv$afl_fixture$status == newFixture$status)){
      # Trigger Fanfooty Refresh
      rv$refreshFanfooty = rv$refreshFanfooty + 1
    } else {
      # Update afl fixture locally and globally
      rv$afl_fixture <- newFixture
      g$afl_fixture <<-newFixture
      # Trigger supercoach refresh
      rv$refreshSupercoach = ifelse(is.null(rv$refreshSupercoach),0,rv$refreshSupercoach) + 1
    }
  })
  
  # Update Supercoach data
  observeEvent(rv$refreshSupercoach, {
    
    # Update player data locally and globally
    rv$player_data <- get_player_data(rv$sc, rv$vRound)
    g$player_data <<- rv$player_data
    
  })
  
  # filter for the live games
  ff_live_games_r <- reactive({
    
    # Create static versions
    afl_fixture <- rv$afl_fixture
    ff_fixture <- rv$ff_fixture
    
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
    ff_live_scores <- tibble(
      feed_id = numeric(0),
      live_points = numeric(0),
      live_projection = numeric(0),
      max_tog = numeric(0)
    )
    
    # loop through live games
    for (game_id in ff_live_games$game_id){
      
      # get specific fanfooty data
      ff_game_data <- get_ff_game_data(game_id) %>%
        select(feed_id, supercoach, time_on_ground) %>%
        rename(live_points = supercoach) %>%
        mutate(live_projection = round(live_points * 3300/sum(live_points))) %>%
        mutate(max_tog = max(time_on_ground)) %>%
        select(feed_id, live_points, live_projection, max_tog)
      
      # merge to container
      ff_live_scores <- bind_rows(ff_live_scores,ff_game_data) 
    }
    
    return(ff_live_scores)
  })

  # Create live data tables
  live_data_r <- reactive({
    
    player_data <- rv$player_data
    player_data$points[is.na(player_data$points)] <- 0
    player_data$projected_points[is.na(player_data$projected_points)] <- round(player_data$avg3[is.na(player_data$projected_points)])
    player_data$projected_points[is.na(player_data$projected_points)] <- 0
    
    afl_fixture <- rv$afl_fixture
    ff_live_scores <- ff_live_scores_r()
    
    # Merge supercoach with live stats
    live_data <- player_data %>%
      mutate(name = paste0(substr(first_name,1,1),'.',last_name)) %>%
      select(feed_id, name, team_abbrev,team, coach, picked, type, position, projected_points, points) %>%
      left_join(ff_live_scores, by=c('feed_id'))  %>%
      left_join(afl_fixture[c('team1_abbrev','kickoff','status')], by=c('team_abbrev'='team1_abbrev')) %>%
      arrange(coach, desc(type), desc(picked)) %>%
      group_by(coach) %>%
      mutate(row_num = row_number()) %>%
      filter(row_num <= 18) %>%
      mutate(proj = case_when(status == 'post'                ~ points,
                              status == 'pre'                 ~ projected_points,
                              live_points >= projected_points ~ live_projection,
                              max_tog >= 75                   ~ live_projection,
                              TRUE                            ~ projected_points)) %>%
      mutate(pnts = case_when(status == 'now'                 ~ live_points,
                              TRUE                            ~ points)) %>%
      arrange(coach, match(status, c('now','pre','post')), kickoff, team_abbrev, desc(proj)) %>%
      ungroup() %>%
      select(coach, team, name, team_abbrev, position, proj, pnts) %>%
      rename(projected_points = proj) %>%
      rename(points = pnts)

    return(live_data)
  })
  
  # Create live ladder
  live_ladder_r <- reactive({
    
    live_data <- live_data_r()
    ladder_data <- rv$ladder_data
    fixture_data <- rv$fixture_data
    
    live_smy <- live_data %>%
      filter(!is.na(coach)) %>%
      group_by(coach) %>%
      summarise(
        points = sum(projected_points),
        .groups='drop'
      )
    
    fData <- fixture_data %>%
      select(fixture, team, coach, opponent_coach) %>%
      left_join(live_smy, by=c('coach')) %>%
      rename(points_for = points) %>%
      left_join(live_smy, by=c('opponent_coach'='coach')) %>%
      rename(points_against = points) %>%
      mutate(wins = ifelse(points_for > points_against,1,0)) %>%
      mutate(draws = ifelse(points_for == points_against, 1,0)) %>%
      mutate(losses = ifelse(points_for < points_against,1,0)) %>%
      rename(teamname = team)
    
    live_ladder <- bind_rows(ladder_data, fData) %>%
      group_by(coach, teamname) %>%
      summarise(
        W = sum(wins),
        D = sum(draws),
        L = sum(losses),
        PF = sum(points_for),
        PA = sum(points_against),
        .groups='drop'
      ) %>%
      mutate(pcent = round(PF/PA*100,2)) %>%
      mutate(points = (W*4)+(D*2)) %>%
      arrange(desc(points), desc(pcent)) %>%
      mutate(rank = row_number())
    
    tbl <- fData[,c('coach','teamname','points_for','wins')] %>%
      left_join(live_ladder[,c('coach','rank','points','W','pcent')], by=c('coach')) %>%
      left_join(ladder_data[,c('coach','position','pcnt')], by=c('coach')) %>%
      mutate(pos_change = position - rank) %>%
      mutate(pcnt_change = round(pcent - pcnt,digits=2)) %>%
      mutate(pcent = round(pcent,1)) %>%
      arrange(rank) %>%
      select(rank, coach, teamname, points_for, wins, pos_change, W, pcent, pcnt_change)
    
    output <- datatable(
      tbl, 
      class="display compact cell-border stripe",
      rownames = FALSE,
      colnames = c('Coach'='coach',
                   'Proj'='points_for',
                   'Pos Chg' = 'pos_change',
                   'Wins'='W',
                   '%'='pcent',
                   '% Chg' = 'pcnt_change'),
      options = list(
        dom='t',
        lengthChange = FALSE,
        columnDefs = list(
          list(targets= c(0,2,4), visible=F),
          list(targets= c(3,5,6:8), className='dt-center')
        )
      )
    ) %>%
      formatStyle('Proj',
                  'wins',
                  backgroundColor = styleInterval(c(0), c('#F46D43','#66BD63')))
    
    return(output)
  })
  
  displayGame <- function(fixture_data, game_num, live_data){
    
    game_data <- fixture_data %>%
      filter(fixture == game_num)
    
    homeTeam <- live_data %>%
      filter(coach == game_data$coach[1]) %>%
      select(name, team_abbrev, position, projected_points, points)
    
    awayTeam <- live_data %>%
      filter(coach == game_data$opponent_coach[1]) %>%
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
  
  # UI Output
  output$pgGame1 <- renderUI({
    req(live_data_r())
    ui <- displayGame(rv$fixture_data, 1, live_data_r())
    return(ui)
  })
  output$pgGame2 <- renderUI({
    req(live_data_r())
    ui <- displayGame(rv$fixture_data, 2, live_data_r())
    return(ui)
  })
  output$pgGame3 <- renderUI({
    req(live_data_r())
    ui <- displayGame(rv$fixture_data, 3, live_data_r())
    return(ui)
  })
  output$pgGame4 <- renderUI({
    req(live_data_r())
    ui <- displayGame(rv$fixture_data, 4, live_data_r())
    return(ui)
  })
  
  output$tbl = renderDT(live_ladder_r())
    
  
}

shinyApp(ui = ui, server = server)
