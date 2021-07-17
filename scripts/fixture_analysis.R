# Load supercoach functions
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

fixture_data <- get_fixture_data(cid,tkn)



fData <- fixture_data %>%
  filter(team_score > 0 & !is.na(team_score)) %>%
  select(round, fixture, coach, team_score, opponent_coach, opponent_score) %>%
  filter(round != 12 & round != 13 & round != 14)

coaches <- sort(unique(fData$coach))

results <- tibble()
n <- 0
for(c in coaches){
  
  #opponents <- coaches[!grepl(c, coaches)]
  opponents <- coaches
  
  
  for(o in opponents){
    
    n <- n + 1
    
    sim <- fData %>%
      select(round, fixture, coach, team_score)
    
    # swap coaches
    sim$coach[fData$coach == o] <- c 
    sim$coach[fData$coach == c] <- o
    
    #swap scores
    sim$team_score[sim$coach == c] <- fData$team_score[fData$coach ==  c] 
    sim$team_score[sim$coach == o] <- fData$team_score[fData$coach ==  o] 
    
    # Create new fixture
    sim_opp <- sim %>%
      rename(opponent_coach = coach) %>%
      rename(opponent_score = team_score)
    
    sim1 <- sim %>%
      left_join(sim_opp, by=c('round','fixture')) %>%
      filter(coach != opponent_coach)
    
    sim2 <- sim1 %>%
      mutate(differential = team_score - opponent_score) %>%
      mutate(win = ifelse(differential > 0, 1,0)) %>%
      mutate(draw = ifelse(differential == 0, 1,0)) %>%
      mutate(loss = ifelse(differential < 0, 1,0)) %>%
      group_by(coach) %>%
      mutate(cumul_win = cumsum(win)) %>%
      mutate(cumul_draw = cumsum(draw)) %>%
      mutate(cumul_loss = cumsum(loss)) %>%
      mutate(cumul_points_for = cumsum(team_score)) %>%
      mutate(cumul_points_against = cumsum(opponent_score)) %>%
      mutate(points = cumul_win*4 + cumul_draw*2) %>%
      mutate(pcnt = round(cumul_points_for/cumul_points_against*100,2))
      
    sim3 <- sim2 %>%
      filter(round == max(round)) %>%
      ungroup() %>%
      arrange(desc(points), desc(pcnt)) %>%
      mutate(rank = row_number()) %>%
      mutate(who_fixture = c) %>%
      mutate(swapped_with = o) %>%
      mutate(sim = n)
    
    results <- bind_rows(results, sim3)
  }
}

x <- results %>%
  filter(coach == who_fixture) %>%
  select(who_fixture, swapped_with, rank) %>%
  spread(swapped_with, rank) %>%
  adorn_totals(where='col', name = 'Best_Team') %>%
  mutate(Best_Team = round(Best_Team/8,1))

y <- x %>%
  adorn_totals(where='row', name = 'Best_Fixture')

avg <- round(y[y$who_fixture == 'Best_Fixture',2:9]/8,1)
rank <- rank(avg)

avg$who_fixture <- 'Avg Position'
rank$who_fixture <- 'Fixture Rank'

x <- bind_rows(x, avg)
x <- bind_rows(x, rank)

x

x <- results %>%
  filter(coach == who_fixture) %>%
  select(who_fixture, swapped_with, cumul_win) %>%
  spread(swapped_with, cumul_win) %>%
  adorn_totals(where='col', name = 'Best_Team') %>%
  mutate(Best_Team = round(Best_Team/8,1))

y <- x %>%
  adorn_totals(where='row', name = 'Best_Fixture')

avg <- round(y[y$who_fixture == 'Best_Fixture',2:9]/8,1)
rank <- rank(avg)

avg$who_fixture <- 'Avg Position'
rank$who_fixture <- 'Fixture Rank'

x <- bind_rows(x, avg)
x <- bind_rows(x, rank)

x


