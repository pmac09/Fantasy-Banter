source('./R/fantasy-banter-functions.R')

szn <- sc$var$season
league_data <- fb_league() 

last_year <- league_data %>%
  filter(season == szn) %>%
  summarise(
    mean = mean(team_score, na.rm=T),
    sd = sd(team_score, na.rm=T)
  )

sim_data <- league_data %>%
  filter(season == szn) %>%
  select(season, round, fixture, coach) %>%
  cross_join(tibble(sim = c(1:10000)))

sim_data$team_score <- round(rnorm(nrow(sim_data), last_year$mean, last_year$sd),0)

oppo_data <- sim_data
names(oppo_data)[grepl('coach',names(sim_data))] <- 'oppo_coach'
names(oppo_data)[grepl('team_score',names(sim_data))] <- 'opponent_score'

sim_data2 <- sim_data %>%
  left_join(oppo_data, by=c('season','round','fixture','sim'), relationship = "many-to-many") %>%
  filter(coach != oppo_coach) %>%
  mutate(differential = team_score - opponent_score) %>%
  mutate(win = ifelse(differential > 0, 1,0)) %>%
  mutate(draw = ifelse(differential == 0, 1,0)) %>%
  mutate(loss = ifelse(differential < 0, 1,0)) 

sim_data3 <- sim_data2 %>%
  group_by(season, sim, coach) %>%
  summarise(
    wins = sum(win),
    draws = sum(draw),
    losses = sum(loss),
    points_total = sum(team_score),
    points_mean = mean(team_score),
    points_sd = sd(team_score),
    .groups = 'drop'
  ) %>%
  arrange(sim, desc(wins), desc(draws), desc(points_total)) %>%
  group_by(sim) %>%
  mutate(position = row_number()) %>%
  mutate(finals = ifelse(position <= 4,1,0))

sim_data4 <- sim_data3 %>%
  select(season, sim, coach, position, points_mean, points_sd) %>%
  filter(position <= 4) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup()%>%
  mutate(fixture = ifelse(position <= 2, 1, 2)) %>%
  group_by(season,sim,fixture) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()
    
sim_data5 <- sim_data4 %>%
  filter((fixture == 1 & win == FALSE) | (fixture == 2 & win == TRUE)) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup() %>%
  group_by(season,sim) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()

sim_data6 <- bind_rows(
  (sim_data4 %>% filter(fixture == 1 & win == TRUE)),
  (sim_data5 %>% filter(win == TRUE))
) %>%
  arrange(season, sim) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup() %>%
  group_by(season,sim) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()

results <- sim_data3 %>% 
  left_join(sim_data6[,c('season','sim','coach','win')], by=c('season','sim','coach')) %>%
  mutate(gfinal = ifelse(!is.na(win)==TRUE,1,0)) %>%
  mutate(champ = ifelse(win==TRUE,1,0)) %>%
  group_by(coach) %>%
  summarise(
    sims = n(),
    mean = round(mean(position),2),
    finals = round(sum(finals)/n(),3),
    gfinal = round(sum(gfinal,na.rm=T)/n(),3),
    champ = round(sum(champ,na.rm=T)/n(),3),
    spoon = round(sum(ifelse(position==8,1,0))/n(),3)
  ) %>%
  arrange(desc(champ))

results

league_data %>%
  filter(is.na(points)) %>%
  group_by(season) %>%
  summarise(
    n_distinct(round)
  )


 
# summary
# write.csv(summary, paste0(savePath,'R',sc$var$current_round,'_sim_smy.csv'), na='', row.names = F)
# 
# 
# pos_pcnt <- results %>%
#   group_by(coach, position) %>%
#   summarise(n=n(),
#             .groups='drop') %>%
#   ungroup() %>%
#   group_by(coach) %>%
#   mutate(pcnt = round(n/sum(n)*100)) 
# 
# pos_pcnt %>%
#   select(-n) %>%
#   arrange(position) %>%
#   pivot_wider(names_from=position, names_prefix='pos_',values_from='pcnt') %>%
#   arrange(desc(pos_1),desc(pos_2),desc(pos_3),desc(pos_4),desc(pos_5),desc(pos_6),desc(pos_7),desc(pos_8))
# 
# 
# 
# 
# x <- results %>%
#   group_by(position, wins) %>%
#   summarise(n=n(),
#             .groups='drop') %>%
#   ungroup() %>%
#   group_by(wins) %>%
#   arrange(desc(wins)) %>%
#   mutate(pcnt = round(n/sum(n)*100)) %>%
#   select(-n) %>%
#   pivot_wider(names_from=position, names_prefix='pos_',values_from='pcnt')
# 
# x
# 
# 
# results %>%
#   filter(position<=4) %>%
#   select(coach,position, simulation) %>%
#   pivot_wider(names_from = position, values_from = coach) %>%
#   mutate(G1 = ifelse(`1`<`4`,paste0(`1`,' v ',`4`),paste0(`4`,' v ',`1`))) %>%
#   mutate(G2 = ifelse(`2`<`3`,paste0(`2`,' v ',`3`),paste0(`3`,' v ',`2`))) %>%
#   select(simulation, G1,G2) %>%
#   pivot_longer(cols=starts_with('G')) %>%
#   group_by(value) %>%
#   summarise(
#     n=n()
#   ) %>%
#   ungroup() %>%
#   mutate(pcnt = round(n/sum(n)*100,1)) %>%
#   arrange(desc(pcnt))
# 
# 
# a <- results %>%
#   filter((coach == "Luke" & position > 4))
# 
# b <- results %>%
#   filter((coach == "Simon" & position > 4))
# 
# c <- results %>%
#   filter((coach == "Jordan" & position > 4))
# 
# d <- a %>%
#   bind_rows(b) %>%
#   bind_rows(c) %>%
#   group_by(simulation) %>%
#   summarise(n=n()) %>%
#   arrange(desc(n))
# 
# results %>%
#   filter(simulation == 475)


league_data <- fb_league()
sim_results <- tibble() 

for (szn in c(2025:2025)){

last_year <- league_data %>%
  filter(season == max(szn-1,2016)) %>%
  summarise(
    mean = mean(team_score, na.rm=T),
    sd = sd(team_score, na.rm=T)
  )


szn_data <- league_data %>%
  filter(season == szn) 


for (rnd in unique(szn_data$round[!is.na(szn_data$points)])){
 
sim_data <- szn_data %>%
  select(season, round, fixture, coach, team_score) %>%
  mutate(team_score = ifelse(round>rnd,NA,team_score)) %>%
  cross_join(tibble(sim = c(1:100000))) 

na_idx <- is.na(sim_data$team_score)
sim_data$team_score[na_idx] <- round(rnorm(sum(na_idx), last_year$mean, last_year$sd), 0)

oppo_data <- sim_data
names(oppo_data)[grepl('coach',names(sim_data))] <- 'oppo_coach'
names(oppo_data)[grepl('team_score',names(sim_data))] <- 'opponent_score'

sim_data2 <- sim_data %>%
  left_join(oppo_data, by=c('season','round','fixture','sim'), relationship = "many-to-many") %>%
  filter(coach != oppo_coach) %>%
  mutate(differential = team_score - opponent_score) %>%
  mutate(win = ifelse(differential > 0, 1,0)) %>%
  mutate(draw = ifelse(differential == 0, 1,0)) %>%
  mutate(loss = ifelse(differential < 0, 1,0)) 

sim_data3 <- sim_data2 %>%
  group_by(season, sim, coach) %>%
  summarise(
    wins = sum(win),
    draws = sum(draw),
    losses = sum(loss),
    points_total = sum(team_score),
    points_mean = mean(team_score),
    points_sd = sd(team_score),
    .groups = 'drop'
  ) %>%
  arrange(sim, desc(wins), desc(draws), desc(points_total)) %>%
  group_by(sim) %>%
  mutate(position = row_number()) %>%
  mutate(finals = ifelse(position <= 4,1,0))

sim_data4 <- sim_data3 %>%
  select(season, sim, coach, position, points_mean, points_sd) %>%
  filter(position <= 4) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup()%>%
  mutate(fixture = ifelse(position %in% c(1,2), 1, 2)) %>% # fixture
  group_by(season,sim,fixture) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()

sim_data5 <- sim_data4 %>%
  filter((fixture == 1 & win == FALSE) | (fixture == 2 & win == TRUE)) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup() %>%
  group_by(season,sim) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()

sim_data6 <- bind_rows(
  (sim_data4 %>% filter(fixture == 1 & win == TRUE)),
  (sim_data5 %>% filter(win == TRUE))
) %>%
  # sim_data4 %>% 
  # filter(win == TRUE) %>%
  arrange(season, sim) %>%
  rowwise() %>%
  mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
  ungroup() %>%
  group_by(season,sim) %>%
  mutate(win = team_score==max(team_score)) %>%
  ungroup()

results <- sim_data3 %>% 
  left_join(sim_data6[,c('season','sim','coach','win')], by=c('season','sim','coach')) %>%
  mutate(gfinal = ifelse(!is.na(win)==TRUE,1,0)) %>%
  mutate(champ = ifelse(win==TRUE,1,0)) %>%
  group_by(coach) %>%
  summarise(
    sims = n(),
    sim_pos = round(mean(position),2),
    sim_finals = round(sum(finals)/n(),6),
    sim_gfinal = round(sum(gfinal,na.rm=T)/n(),6),
    sim_champ = round(sum(champ,na.rm=T)/n(),6),
    sim_spoon = round(sum(ifelse(position==8,1,0))/n(),6)
  ) %>%
  arrange(sim_pos) %>%
  mutate(round = rnd) %>%
  mutate(season = szn)

print(results)

sim_results <- bind_rows(sim_results, results)


}

}


x<-league_data %>%
  left_join(sim_results, by=c('season','round','coach')) %>%
   select(-sims)



firebase_upload(databaseURL, '/Fantasy-Banter/data/league', x, cols=TRUE)


y <- league_data %>% 
  filter(season == 2025)



season_simulation <- function(league_data, szn, rnd) {
  
  last_year <- league_data %>%
    filter(season == max(szn-1,2016)) %>%
    summarise(
      mean = mean(team_score, na.rm=T),
      sd = sd(team_score, na.rm=T)
    )
  
  szn_data <- league_data %>%
    filter(season == szn) 
  
  sim_data <- szn_data %>%
    select(season, round, fixture, coach, team_score) %>%
    mutate(team_score = ifelse(round>rnd,NA,team_score)) %>%
    cross_join(tibble(sim = c(1:100000))) 
  
  na_idx <- is.na(sim_data$team_score)
  sim_data$team_score[na_idx] <- round(rnorm(sum(na_idx), last_year$mean, last_year$sd), 0)
  
  oppo_data <- sim_data
  names(oppo_data)[grepl('coach',names(sim_data))] <- 'oppo_coach'
  names(oppo_data)[grepl('team_score',names(sim_data))] <- 'opponent_score'
  
  sim_data2 <- sim_data %>%
    left_join(oppo_data, by=c('season','round','fixture','sim'), relationship = "many-to-many") %>%
    filter(coach != oppo_coach) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0)) 
  
  sim_data3 <- sim_data2 %>%
    group_by(season, sim, coach) %>%
    summarise(
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss),
      points_total = sum(team_score),
      points_mean = mean(team_score),
      points_sd = sd(team_score),
      .groups = 'drop'
    ) %>%
    arrange(sim, desc(wins), desc(draws), desc(points_total)) %>%
    group_by(sim) %>%
    mutate(position = row_number()) %>%
    mutate(finals = ifelse(position <= 4,1,0))
  
  sim_data4 <- sim_data3 %>%
    select(season, sim, coach, position, points_mean, points_sd) %>%
    filter(position <= 4) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup()%>%
    mutate(fixture = ifelse(position %in% c(1,2), 1, 2)) %>% 
    group_by(season,sim,fixture) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  sim_data5 <- sim_data4 %>%
    filter((fixture == 1 & win == FALSE) | (fixture == 2 & win == TRUE)) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup() %>%
    group_by(season,sim) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  sim_data6 <- bind_rows(
    (sim_data4 %>% filter(fixture == 1 & win == TRUE)),
    (sim_data5 %>% filter(win == TRUE))
  ) %>%
    # sim_data4 %>% 
    # filter(win == TRUE) %>%
    arrange(season, sim) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup() %>%
    group_by(season,sim) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  results <- sim_data3 %>% 
    left_join(sim_data6[,c('season','sim','coach','win')], by=c('season','sim','coach')) %>%
    mutate(gfinal = ifelse(!is.na(win)==TRUE,1,0)) %>%
    mutate(champ = ifelse(win==TRUE,1,0)) %>%
    group_by(coach) %>%
    summarise(
      sims = n(),
      sim_pos = round(mean(position),2),
      sim_finals = round(sum(finals)/n(),6),
      sim_gfinal = round(sum(gfinal,na.rm=T)/n(),6),
      sim_champ = round(sum(champ,na.rm=T)/n(),6),
      sim_spoon = round(sum(ifelse(position==8,1,0))/n(),6)
    ) %>%
    arrange(sim_pos) %>%
    mutate(round = rnd) %>%
    mutate(season = szn)
  
  return(results)
  
}

season_simulation(league_data, 2025,19)

