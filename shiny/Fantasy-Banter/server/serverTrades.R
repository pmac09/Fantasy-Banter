### FUNCTIONS ------------------------------------------------------------------

get_team_names <- function(league_data){
  print_log('get_team_names')
  team_names <- lapply(league_data$ladder, function(x) x$userTeam$id)
  names(team_names) <- lapply(league_data$ladder, function(x) as.character(x$userTeam$teamname))  
  ordered <- order(names(team_names))
  team_names <- team_names[ordered]
  return(team_names)
}
get_team_players <- function(sc, team_names){
  team_players <- lapply(team_names, function(x){
    data <- sc_team(sc, x, sc$var$next_round)$players
    
    data1 <- tibble(
      user_team_id = unlist(lapply(data, function(x){x$user_team_id})),
      player_id = unlist(lapply(data, function(x){x$player_id}))
    )
    
    return(data1)

  })
  team_players1 <- bind_rows(team_players)
  return(team_players1)
}
get_player_data <- function(sc, team_players){
  
  player_list <- sc_players(sc)
  
  player_data <- tibble(
    id = unlist(lapply(player_list, function(x) x$id)),
    feed_id = unlist(lapply(player_list, function(x) x$feed_id)),
    player_name = NA,
    first_name = unlist(lapply(player_list, function(x) x$first_name)),
    last_name = unlist(lapply(player_list, function(x) x$last_name)),
    team = unlist(lapply(player_list, function(x) x$team$abbrev)),
    pos = unlist(lapply(player_list, function(x) paste0(lapply(x$positions, function(p) p$position), collapse = '/'))),
    avg =  unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg)),
    avg3 = unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg3)),
    avg5 = unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg5))
  ) %>%
    left_join(team_players, by=c('id'='player_id')) %>%
    mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
    group_by(player_name, team) %>%
    mutate(n = n()) %>%
    mutate(player_name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), player_name)) %>%
    ungroup() %>%
    select(-n, -first_name)
  
  return(player_data)
}
get_player_names <- function(player_data, team_id){
  print_log('get_player_names')
  
  team_data <- player_data %>%
    filter(user_team_id == team_id)
  
  player_names <- team_data$id
  names(player_names) <- team_data$player_name
  player_names <- player_names[order(team_data$last_name)]
  
  return(player_names)
}
get_team_data <- function(player_data, team_id, give_player_ids, get_player_ids, pos_list){
  
  
  pre_team <- player_data %>%
    filter(user_team_id == team_id) %>%
    select(id, player_name, team, pos, avg) 
  
  pre_team2 <- pre_team %>%
    left_join(get_best_team(pre_team, pos_list), by=c('id')) %>%
    left_join(pos_list, by=c('best_pos' = 'pos')) %>%
    arrange(desc(avg)) %>%
    group_by(best_pos) %>%
    mutate(bench = row_number() > n) %>%
    arrange(bench, desc(avg)) %>%
    select(id, best_pos, bench) %>%
    rename(pre_pos = best_pos) %>%
    rename(pre_bench = bench)
    
  post_team <- player_data %>%
    filter(user_team_id == team_id | id %in% get_player_ids) %>%
    filter(!(id %in% give_player_ids)) %>%
    select(id, player_name, team, pos, avg) 
  
  post_team2 <- post_team %>%
    left_join(get_best_team(post_team, pos_list), by=c('id')) %>%
    left_join(pos_list, by=c('best_pos' = 'pos')) %>%
    arrange(desc(avg)) %>%
    group_by(best_pos) %>%
    mutate(bench = row_number() > n) %>%
    arrange(bench, desc(avg)) %>%
    filter(row_number() <= 23) %>%
    select(id, best_pos, bench) %>%
    rename(post_pos = best_pos) %>%
    rename(post_bench = bench)
    
  team_data <- player_data %>%
    filter(id %in% c(pre_team2$id, post_team2$id) ) %>%
    arrange(desc(avg)) %>%
    select(id, player_name, team, pos, avg) %>%
    left_join(pre_team2,  by=c('id')) %>%
    left_join(post_team2,  by=c('id')) 
    
}
get_best_team <- function(team_data, pos_list){
  print_log('get_best_team')
  
  distinct_pos <- strsplit(team_data$pos, '/') 
  combos <- expand.grid(distinct_pos)
  
  combo_avg <- apply(combos, 1,  function(x){
    y <- tibble(
      id = as.numeric(names(x)),
      pos = x
    ) %>%
      left_join(team_data[,c('id','avg')], by=c('id')) %>%
      arrange(desc(avg)) %>%
      group_by(pos) %>%
      mutate(rank = row_number()) %>%
      left_join(pos_list, by=c('pos')) %>%
      filter(rank <= n) %>%
      ungroup()
    
    return(sum(y$avg))
  })
  combo_best <- order(-combo_avg)[1]
  
  combo <- tibble(
    id = as.numeric(names(combos)),
    best_pos = unlist(combos[combo_best,])
  )
  
  return(combo)
}
get_team_totals <- function(team_data){
  
  pre_trade <- team_data %>%
    filter(!is.na(pre_pos)) %>%
    mutate(pos = case_when(pre_bench==TRUE ~ 'BENCH',
                           pre_pos == 'DEF' ~ 'DEFENCE',
                           pre_pos == 'MID' ~ 'MIDFIELD',
                           pre_pos == 'RUC' ~ 'RUCK',
                           pre_pos == 'FWD' ~ 'FORWARD')) %>%
    group_by(pos) %>%
    summarise(pre_trade = sum(avg))
  
  post_trade <- team_data %>%
    filter(!is.na(post_pos)) %>%
    mutate(pos = case_when(post_bench==TRUE ~ 'BENCH',
                           post_pos == 'DEF' ~ 'DEFENCE',
                           post_pos == 'MID' ~ 'MIDFIELD',
                           post_pos == 'RUC' ~ 'RUCK',
                           post_pos == 'FWD' ~ 'FORWARD')) %>%
    group_by(pos) %>%
    summarise(post_trade = sum(avg))
  
  team_totals <- pre_trade %>%
    left_join(post_trade, by=c('pos')) %>%
    mutate(diff = post_trade - pre_trade) %>%
    arrange(match(substr(pos,1,1), c('D','M','R','F','B'))) 
    
  return(team_totals)
}


# get_player_img <- function(player_id){
#   url <- paste0('https://s.afl.com.au/staticfile/AFL%20Tenant/AFL/Players/ChampIDImages/AFL/2024014/%s.png')
#   return(url)
# }

### STATIC -- ------------------------------------------------------------------

league_data  <- sc_league(sc)
team_names   <- get_team_names(league_data)
team_players <- get_team_players(sc,team_names)
player_data  <- get_player_data(sc, team_players)

pos_list <- tibble(
  pos = c('DEF','MID','RUC','FWD'),
  n = c(5,7,2,5)
)

### REACTIVE -------------------------------------------------------------------

output$uiSelectTeamLeft <- renderUI({
  selectInput('selectTeamLeft','Select Team',team_names)
})
output$uiSelectPlayersLeft <- renderUI({
  req(input$selectTeamLeft)
  
  team_id <- team_names[[3]]
  team_id <- input$selectTeamLeft
  
  player_names <- get_player_names(player_data, team_id)
  selectizeInput('selectPlayersLeft', 'Trade Players', choices = player_names, multiple = TRUE)
})
teamLeft <- reactive({
  req(input$selectTeamLeft)
  team_id <- team_names[[1]]
  team_id <- input$selectTeamLeft
  team_data <- get_team_data(player_data, team_id, input$selectPlayersLeft, input$selectPlayersRight, pos_list)
  return(team_data)
})
output$uiTeamSummaryLeft <- renderUI({
  req(input$selectTeamLeft)
  teamSummaryDisplay(get_team_totals(teamLeft()))
})
output$uiTeamDisplayLeft <- renderUI({
  req(input$selectTeamLeft)
  teamDisplay(teamLeft())
})

output$uiSelectTeamRight <- renderUI({
  selectInput('selectTeamRight','Select Team',rev(team_names))
})
output$uiSelectPlayersRight <- renderUI({
  req(input$selectTeamRight)
  
  team_id <- team_names[[1]]
  team_id <- input$selectTeamRight
  
  player_names <- get_player_names(player_data, team_id)
  selectizeInput('selectPlayersRight', 'Trade Players', choices = player_names, multiple = TRUE)
})
teamRight = reactive({
  req(input$selectTeamRight)
  
  team_id <- team_names[[1]]
  team_id <- input$selectTeamRight
  
  team_data <- get_team_data(player_data, team_id, input$selectPlayersRight, input$selectPlayersLeft, pos_list)
  
  return(team_data)
})
output$uiTeamSummaryRight <- renderUI({
  req(input$selectTeamRight)
  teamSummaryDisplay(get_team_totals(teamRight()))
})
output$uiTeamDisplayRight <- renderUI({
  req(input$selectTeamRight)
  teamDisplay(teamRight())
})


### OBSERVES -------------------------------------------------------------------

observeEvent(input$selectTeamLeft, {
  print(input$selectTeamLeft)
})
observeEvent(input$selectPlayersLeft, {
  print(input$selectPlayersLeft)
})


### UI Functions ---------------------------------------------------------------

teamDisplay <- function(team_data){

  ui <- column(
    width=12,
    posDisplay(team_data, 'DEF'),
    posDisplay(team_data, 'MID'),
    posDisplay(team_data, 'RUC'),
    posDisplay(team_data, 'FWD')
  )
  
  return(ui)
}
posDisplay <- function(team_data, display_pos){
  
  header <- case_when(
    display_pos == 'DEF' ~ 'DEFENCE',
    display_pos == 'MID' ~ 'MIDFIELD',
    display_pos == 'RUC' ~ 'RUCK',
    display_pos == 'FWD' ~ 'FORWARD'
  )
  
  pos_data <- team_data %>%
    filter(coalesce(post_pos,pre_pos) == display_pos)
  
  ui <- list(
    h4(header),
    wellPanel(
      style = "overflow-y:scroll; background:white;", 
      productList(
        apply(pos_data, 1, playerDisplay)
      )
    )
  )
  
  return(ui)
}
playerDisplay <- function(pos_data){

  pos_data <- as.list(pos_data)
  
  name <- pos_data$player_name
  team <- pos_data$team
  stat <- pos_data$avg
  
  team_img <- img(src = paste0('./team colours/',team,'.png'))

  flag_css <- ifelse(pos_data$post_bench == TRUE, 'background-color:LightGray;', '')
  flag_css <- paste0(flag_css, ifelse(is.na(pos_data$pre_pos) & !is.na(pos_data$post_pos), 'font-weight: bold; color:green;', ''))
  flag_css <- paste0(flag_css, ifelse(is.na(pos_data$post_pos), 'font-style: italic; color:LightGray;', ''))
  
  img_css <- ifelse(is.na(pos_data$post_pos), 'opacity: 0.25;', '')
  
  
  shiny::tags$li(
    class = "item", style=flag_css,
    div(
      class='playerItem',
      div(class='playerStat', stat),
      div(class='teamLogo', style = img_css, team_img),
      div(class='playerName', name)
    )
  )
  
}

teamSummaryDisplay <- function(team_totals){
  
  onfield <- tibble(
    pos = 'ON FIELD',
    post_trade = sum(team_totals$post_trade[team_totals$pos!='BENCH']),
    diff = sum(team_totals$diff[team_totals$pos!='BENCH'])
  )
    
  overall <- tibble(
    pos = 'TOTAL',
    post_trade = sum(team_totals$post_trade),
    diff = sum(team_totals$diff)
  )
  
  
  team_totals2 <- bind_rows(
    team_totals,
    onfield,
    overall
  ) %>%
    arrange(match(substr(pos,1,1), c('D','M','R','F','O','B','T')))
  
  
  ui <- column(width=12,
    
    h4('SUMMARY'),
    wellPanel(
      style = "overflow-y:scroll; background:white;", 
      productList(
        apply(team_totals2, 1, positionDisplay)
      )
    )
  )
  
  return(ui)
}
positionDisplay <- function(line_total){

  line_total <- as.list(line_total)
  
  line_total$diff <- round(as.numeric(line_total$diff,1))
  
  diff_css <- case_when(
    line_total$diff == 0 ~ 'color:LightGray',
    line_total$diff > 0 ~ 'font-weight: bold; color:green;',
    line_total$diff < 0 ~ 'font-weight: bold; color:red;'
  )
  
  bold_css <- ifelse(line_total$pos %in% c('ON FIELD', 'TOTAL'), 'font-weight: bold;','')
  
  
  shiny::tags$li(
    class = "item", style=bold_css,
    div(
      class='positionItem',
      div(class='positionResult', line_total$post_trade),
      div(class='positionResult', style=diff_css, line_total$diff),
      div(class='positionName', line_total$pos)
    )
  )

  
}

