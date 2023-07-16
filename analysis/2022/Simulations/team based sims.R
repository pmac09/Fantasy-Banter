
library(zoo)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

savePath <- './data/2023/simulations/'

sc <- get_sc(cid, tkn)

league_data <- get_league_data(sc)
player_data <- get_player_data(sc, round=sc$var$current_round)

# Bye Rounds
# bye_rounds <- c(12,13,14)
# bye_url_end <- paste0('?round=',bye_rounds)
# bye_url <- paste0(sc$url$aflFixture, bye_url_end)
# 
# bye_data <- tibble()
# for(url in bye_url){
#   sc_data <- get_sc_data(sc$auth, url)
#   data <- get_afl_fixture_data(sc_data)
#   bye_data <- bind_rows(bye_data,data)
# }
# 
# bye_teams <- bye_data %>%
#   select(team1_abbrev, round) %>%
#   rename(team=team1_abbrev) %>%
#   pivot_wider(names_from=round, names_prefix = 'R', values_from=round) %>%
#   mutate(bye_round = ifelse(is.na(R12),12,ifelse(is.na(R13),13,14))) %>%
#   select(team,bye_round)
# 
# bye_players <- player_data %>%
#   filter(!is.na(coach)) %>%
#   left_join(bye_teams, by=c('team_abbrev'='team')) %>%
#   group_by(coach, bye_round) %>%
#   count() %>%
#   mutate(pcnt = round((19-max(n-4,0))/19,2))


sim_league_data <- league_data %>%
  # select(-pcnt) %>%
  # left_join(bye_players, by=c('coach','round'='bye_round')) %>%
  mutate(pcnt=ifelse(is.na(pcnt),1,pcnt)) 

scores <- sim_league_data %>%
  filter(round != 12 & round != 13 & round != 14) 

league_mean <- mean(scores$team_score, na.rm=T)
league_sd <- sd(scores$team_score, na.rm=T)

# Calc team weighted Mean & SD
# team_smy <- scores %>%
#   group_by(coach) %>%
#   summarise(
#     team_mean = mean(team_score, na.rm=T),
#     team_sd = sd(team_score, na.rm=T),
#   ) %>%
#   mutate(league_mean = league_mean) %>%
#   mutate(league_sd = league_sd) %>%
#   mutate(round = sc$var$current_round) %>%
#   mutate(rounds = 21) %>%
#   mutate(mean = (team_mean*round/rounds) + (league_mean*(rounds-round)/rounds)) %>%
#   mutate(sd = (team_sd*round/rounds) + (league_sd*(rounds-round)/rounds)) %>%
#   select(coach, mean, sd)
#   
# sim_league_data <- sim_league_data %>%
#   select(round,fixture,team_id,team,coach,team_score,opponent_team_id,opponent_team,opponent_coach,opponent_score,pcnt) %>%
#   left_join(team_smy, by=c('coach'))

sim_league_data <- sim_league_data %>%
  select(round,fixture,team_id,team,coach,team_score,opponent_team_id,opponent_team,opponent_coach,opponent_score,pcnt) %>%
  mutate(mean = league_mean) %>%
  mutate(sd = league_sd) 
  
  

## START SIM ------------------

noOfSims <- 10000

pb   <- txtProgressBar(1, noOfSims, style=3)

results <- tibble()
for( sim in 1:noOfSims){
  
  proj <- sim_league_data %>%
    ungroup() %>%
    rowwise() %>%
    mutate(team_score = ifelse(is.na(team_score), round(rnorm(1, mean, sd)*pcnt), team_score)) %>%
    select(-opponent_score)
  
  oppo  <- proj[,c('round','coach', 'team_score')] %>%
    rename(opponent_score = team_score)
  
  season <- proj %>%
    left_join(oppo, by=c('opponent_coach'='coach', 'round')) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0)) 
  
  season_smy <- season %>%
    group_by(coach) %>%
    summarise(
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss),
      total_points = sum(team_score)
    ) %>%
    arrange(desc(wins), desc(draws), desc(total_points)) %>%
    ungroup() %>%
    mutate(simulation = sim) %>%
    mutate(position = row_number()) %>%
    mutate(finals = ifelse(position <= 4,1,0))
  
  final_scores <- season %>%
    filter(round!=12 & round!=13 & round!=14) %>%
    group_by(coach) %>%
    summarise(
      team_mean = mean(team_score),
      team_sd = sd(team_score)
    ) %>%
    full_join(tibble(round=c(1:2)), by=character(0)) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, team_mean, team_sd)))
  
  semi1 <- season_smy %>%
    filter(position == 1 | position == 4) %>%
    left_join(final_scores, by=c('coach')) %>%
    filter(round==1) %>%
    arrange(desc(team_score))
  
  semi2 <- season_smy %>%
    filter(position == 2 | position == 3) %>%
    left_join(final_scores, by=c('coach')) %>%
    filter(round==1) %>%
    arrange(desc(team_score))
  
  final <- final_scores %>%
    filter(round==2) %>%
    filter(coach == semi1$coach[1] | coach == semi2$coach[1]) %>%
    arrange(desc(team_score)) %>%
    mutate(grand_final = 1) %>%
    ungroup() %>%
    mutate(champ = ifelse(team_score==max(team_score),1,0)) %>%
    select(coach, grand_final, champ)
  
  res <- season_smy %>%
    left_join(final, by=c('coach'))
  
  res[is.na(res)] <- 0
  
  results <- rbind(results, res)
  
  setTxtProgressBar(pb, sim)
}

write.csv(results, paste0(savePath,'R',sc$var$current_round,'_sim.csv'), na='', row.names = F)


summary <- results %>%
  mutate(TOP4 = ifelse(position <= 4, 1,0)) %>%
  group_by(coach) %>%
  summarise(
    RANK.MEAN = mean(position),
    WIN.MAX = max(wins),
    WIN.MIN = min(wins),
    FINALS = sum(finals),
    GFINAL = sum(grand_final),
    CHAMP = sum(champ),
    SPOON = sum(ifelse(position==8,1,0)),
    sim = n_distinct(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(pcnt_finals = round(FINALS/sim,3)) %>%
  mutate(pcnt_gfinal = round(GFINAL/sim,3)) %>%
  mutate(pcnt_champ = round(CHAMP/sim,3)) %>%
  mutate(pcnt_spoon = round(SPOON/sim,3)) %>%
  arrange(desc(pcnt_champ))

summary
write.csv(summary, paste0(savePath,'R',sc$var$current_round,'_sim_smy.csv'), na='', row.names = F)


pos_pcnt <- results %>%
  group_by(coach, position) %>%
  summarise(n=n(),
            .groups='drop') %>%
  ungroup() %>%
  group_by(coach) %>%
  mutate(pcnt = round(n/sum(n)*100)) 

pos_pcnt %>%
  select(-n) %>%
  arrange(position) %>%
  pivot_wider(names_from=position, names_prefix='pos_',values_from='pcnt') %>%
  arrange(desc(pos_1),desc(pos_2),desc(pos_3),desc(pos_4),desc(pos_5),desc(pos_6),desc(pos_7),desc(pos_8))




x <- results %>%
  group_by(position, wins) %>%
  summarise(n=n(),
            .groups='drop') %>%
  ungroup() %>%
  group_by(wins) %>%
  arrange(desc(wins)) %>%
  mutate(pcnt = round(n/sum(n)*100)) %>%
  select(-n) %>%
  pivot_wider(names_from=wins, names_prefix='wins_',values_from='pcnt')

x


results %>%
  filter(position<=4) %>%
  select(coach,position, simulation) %>%
  pivot_wider(names_from = position, values_from = coach) %>%
  mutate(G1 = ifelse(`1`<`4`,paste0(`1`,' v ',`4`),paste0(`4`,' v ',`1`))) %>%
  mutate(G2 = ifelse(`2`<`3`,paste0(`2`,' v ',`3`),paste0(`3`,' v ',`2`))) %>%
  select(simulation, G1,G2) %>%
  pivot_longer(cols=starts_with('G')) %>%
  group_by(value) %>%
  summarise(
    n=n()
  ) %>%
  ungroup() %>%
  mutate(pcnt = round(n/sum(n)*100,1)) %>%
  arrange(desc(pcnt))





