# Load supercoach functions
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

sc <- get_sc(cid, tkn)
league_data <- get_league_data(sc)
## START SIM ------------------

scores <- league_data %>% 
  filter(round != 12 & round != 13 & round != 14) %>%
  group_by(coach) %>%
  summarise(
    mean = mean(team_score),
    sd = sd(team_score)
  ) %>%
  ungroup()

finals_data <- get_league_data(sc, 22)

noOfSims <- 10000

pb   <- txtProgressBar(1, noOfSims, style=3)

results <- tibble()
for( sim in 1:noOfSims){
  
  proj <- finals_data %>%
    left_join(scores, by=c('coach')) %>%
    rowwise() %>%
    mutate(team_score =  round(rnorm(1, mean, sd))) %>%
    dplyr::select(round, fixture, coach, mean, sd, team_score, opponent_coach)
  
  proj$ladder_rank <- c(1,2,3,4)
  
  oppo  <- proj[,c('coach', 'team_score')] %>%
    rename(opponent_score = team_score)
  
  proj2 <- proj %>%
    left_join(oppo, by=c('opponent_coach'='coach')) %>%
    mutate(differential = team_score - opponent_score) %>%
    arrange(desc(differential), ladder_rank) %>%
    group_by(fixture) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  proj3 <- proj2 %>%
    filter(rank == 1) %>%
    dplyr::select(coach, ladder_rank, mean, sd) %>%
    rowwise() %>%
    mutate(team_score =  round(rnorm(1, mean, sd))) %>%
    ungroup()
  
  oppo2  <- proj3[,c('coach', 'team_score')] %>%
    rename(opponent_coach = coach)%>%
    rename(opponent_score = team_score)
  
  proj4 <- proj3 %>%
    full_join(oppo2, by=character()) %>%
    filter(coach != opponent_coach) %>%
    mutate(differential = team_score - opponent_score) %>%
    arrange(desc(differential), ladder_rank) %>%
    mutate(rank = row_number()) %>%
    mutate(simulation = sim)
  
  results <- rbind(results, proj4)
  
  setTxtProgressBar(pb, sim)
}

smy <- results %>%
  group_by(coach) %>%
  summarise(
    Sims = noOfSims,
    GF = n(),
    DB = sum(ifelse(rank==1,1,0)),
    .groups='drop'
  ) %>%
  mutate(Grand_Finalist = round(GF/Sims*100,1)) %>%
  mutate(Champion = round(DB/Sims*100,1)) %>%
  dplyr::select(coach, Sims, Grand_Finalist, Champion) %>%
  arrange(desc(Champion),desc(Grand_Finalist))



