
# Import Functions
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

# Get SC settings
settings_data <- get_settings_data(cid, tkn)
sc_auth <- settings_data$sc_auth
vSeason <- 2021
vRound <- settings_data$next_round

# Get AFL Fixture
afl_fixture <- get_afl_fixture(sc_auth, vRound)
afl_fixture <- get_afl_fixture_data(afl_fixture)

# Get SC fixture_data
fixture_data <- get_fixture_data(cid, tkn, vRound)

# Get SC player Data
player_data <- get_player_data(cid, tkn, vRound)

# Build Live SC table
player_data <- player_data %>% 
  left_join(afl_fixture[,c('team1_abbrev','game_num')], by=c('team_abbrev'='team1_abbrev')) %>%
  arrange(coach, kickoff)
  
# Get fixture
ff_fixture <- get_ff_fixture_data(vSeason = vSeason, vRound = vRound)

# get Live games
ff_live <- ff_fixture %>%
  left_join(afl_fixture, by=c('home_team'='team1')) %>%
  filter(status == 'now')

#loop through games
live_scores <- tibble()
for (game_id in ff_live$game_id){
  ff_game_data <- get_ff_game_data(game_id) %>%
    select(feed_id, supercoach) %>%
    rename(live_points = supercoach) %>%
    mutate(live_projection = round(live_points * 3300/sum(live_points)))
  live_scores <- bind_rows(live_scores,ff_game_data) 
}

live_data <- player_data %>%
  left_join(live_scores, by=c('feed_id')) %>%
  select(feed_id, last_name, team_abbrev, game_num, status,
         team, coach, picked, type, position,
         projected_points, points, live_points, live_projection)


x <- live_data %>%
  filter(coach == 'Paul')



