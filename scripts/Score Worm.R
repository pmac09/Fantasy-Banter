##########################################
## WIP Testing

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

sc_auth <- get_sc_auth(cid, tkn)

sc_settings <- get_sc_settings(sc_auth)
round <- sc_ysettings$competition$current_round

sc_me <- get_sc_me(sc_auth)
user_id <- sc_me$id

sc_user <- get_sc_user(sc_auth, user_id)
league_id <- sc_user$draft[[1]]$leagues[[1]]$id

player_data <- get_player_data(cid, tkn, round)
fixture_data <- get_fixture_data(cid, tkn, round)

afl_fixture <- get_afl_fixture_data(get_afl_fixture(sc_auth, round))


vGames <- bind_rows(
  afl_fixture[,c('game_num', 'team1')] %>% rename(team = team1),
  afl_fixture[,c('game_num', 'team2')] %>% rename(team = team2)
  ) %>%
  arrange(game_num)

vTeam1 <- player_data %>%
  filter(coach == fixture_data$coach[1]) %>%
  filter(type == 'scoring') %>%
  left_join(vGames, by=c('team_abbrev'='team')) %>%
  arrange(game_num, desc(points)) %>%
  group_by(game_num) %>%
  mutate(n = rank(-points)) %>%
  ungroup() %>%
  mutate(sum = cumsum(points)) %>%
  mutate(app = round(sum/row_number(),1)) %>%
  select(coach, last_name, points, game_num, n, app)

vTeam2 <- player_data %>%
  filter(coach == fixture_data$opponent_coach[1]) %>%
  filter(type == 'scoring') %>%
  left_join(vGames, by=c('team_abbrev'='team')) %>%
  arrange(game_num, desc(points)) %>%
  group_by(game_num) %>%
  mutate(n = rank(-points)) %>%
  ungroup() %>%
  mutate(sum = cumsum(points)) %>%
  mutate(app = round(sum/row_number(),1)) %>%
  select(coach, last_name, points, game_num, n, app)
  

x <- full_join(vTeam1, vTeam2, by=c('game_num', 'n')) %>%
  add_row(tibble_row(app.x=0, app.y=0, game_num=0), .before=1) %>%
  arrange(game_num, n) %>%
  mutate(app.x = na.locf(app.x)) %>%
  mutate(app.y = na.locf(app.y)) %>%
  mutate(diff = app.x - app.y)

highchart() %>%
  hc_chart(type='line') %>%
  hc_add_series(data=x$diff,
                step=TRUE,
                color='#0088FF',
                negativeColor= '#FF0000')


vGame <- 2
vT1 <- player_data %>%
  filter(coach == fixture_data$coach[vGame]) %>%
  filter(type == 'scoring') %>%
  left_join(vGames, by=c('team_abbrev'='team')) %>%
  group_by(coach, game_num) %>%
  summarise(
    n = n(),
    points = sum(points),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(cumul_n = cumsum(n)) %>%
  mutate(cumul_points = cumsum(points)) %>%
  mutate(avg_pts = round(cumul_points/cumul_n,1)) %>%
  select(game_num, cumul_n, cumul_points)

vT2 <- player_data %>%
  filter(coach == fixture_data$opponent_coach[vGame]) %>%
  filter(type == 'scoring') %>%
  left_join(vGames, by=c('team_abbrev'='team')) %>%
  group_by(coach, game_num) %>%
  summarise(
    n = n(),
    points = sum(points),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(cumul_n = cumsum(n)) %>%
  mutate(cumul_points = cumsum(points)) %>%
  mutate(avg_pts = round(cumul_points/cumul_n,1)) %>%
  select(game_num, cumul_n, cumul_points)


score_worm <- afl_fixture %>%
  mutate(matchup = paste0(team1, ' v ', team2)) %>%
  left_join(vT1, by=c('game_num')) %>%
  left_join(vT2, by=c('game_num')) %>%
  add_row(tibble_row(matchup='', cumul_n.x=0, cumul_points.x=0,cumul_n.y=0, cumul_points.y=0,), .before=1) %>%
  mutate(across(c(cumul_n.x,cumul_points.x,cumul_n.y,cumul_points.y), na.locf)) %>%
  mutate(avg.x = ifelse(!cumul_n.x, 0, round(cumul_points.x/cumul_n.x,1))) %>%
  mutate(avg.y = ifelse(!cumul_n.y, 0, round(cumul_points.y/cumul_n.y,1))) %>%
  mutate(diff = avg.x - avg.y) %>%
  rowwise() %>%
  mutate(margin = round(diff * min(cumul_n.x, cumul_n.y))) %>%
  ungroup() 
  
  

highchart() %>%
  hc_chart(type='line') %>%
  hc_legend(enabled=FALSE) %>%
  hc_yAxis(max=ceiling(max(abs(score_worm$margin))/10)*10,
           min=-ceiling(max(abs(score_worm$margin))/10)*10) %>%
  hc_xAxis(categories = score_worm$matchup) %>%
  hc_add_series(data=score_worm$margin,
                step=TRUE,
                color='#0088FF',
                negativeColor= '#FF0000')



