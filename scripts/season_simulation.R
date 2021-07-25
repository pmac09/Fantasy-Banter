

# Load supercoach functions
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

sc <- get_sc(cid, tkn)
league_data <- get_league_data(sc)
## START SIM ------------------

scores <- league_data %>% 
  filter(round != 12 & round != 13 & round != 14) 

mean <- mean(scores$team_score, na.rm=T)
sd <- sd(scores$team_score, na.rm=T)/1.5

noOfSims <- 10000

pb   <- txtProgressBar(1, noOfSims, style=3)

results <- tibble()
for( sim in 1:noOfSims){
  
  proj <- league_data %>%
    ungroup() %>%
    rowwise() %>%
    mutate(team_score = ifelse(is.na(team_score), round(rnorm(1, mean, sd)), team_score)) %>%
    select(-opponent_score)
  
  oppo  <- proj[,c('round','coach', 'team_score')] %>%
    rename(opponent_score = team_score)
  
  proj <- proj %>%
    left_join(oppo, by=c('opponent_coach'='coach', 'round')) %>%
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
  
  res <- proj %>%
    ungroup() %>%
    filter(round == max(round)) %>%
    arrange(desc(points), desc(pcnt)) %>%
    mutate(simulation = sim) %>%
    mutate(pos = row_number()) %>%
    select(simulation, pos, team, coach, cumul_win, points, pcnt)
  
  results <- rbind(results, res)
  
  setTxtProgressBar(pb, sim)
}

 write.csv(results, '/Users/paulmcgrath/Github/Fantasy-Banter/data/2021/simulation_R19.csv', na='', row.names = F)

summary <- results %>%
  mutate(TOP4 = ifelse(pos <= 4, 1,0)) %>%
  group_by(coach) %>%
  summarise(
    RANK.MEAN = mean(pos),
    WIN.MAX = max(cumul_win),
    WIN.MIN = min(cumul_win),
    FINALS = sum(TOP4),
    sim = n_distinct(simulation)
  ) %>%
  arrange(RANK.MEAN) %>%
  mutate(PCNT = round(FINALS/sim*100))

summary

bkp <- results


smy <- results %>%
  group_by(coach) %>%
  mutate(mean = mean(pos)) %>%
  group_by(coach, pos) %>%
  summarise(mean = mean(mean),
            n=n(),
            .groups='drop') %>%
  mutate(pcnt = round(n/noOfSims*100)) %>%
  select(-n) %>%
  spread(pos, pcnt) %>%
  arrange(mean)

smy


results %>%
  group_by(simulation) %>%
  summarise(

  )


c <- results[,c('simulation','pos','coach')] %>%
  filter(pos == 3 | pos == 4) %>%
  spread(pos, coach) %>%
  pivot_longer(cols=c('3','4')) %>%
  select(simulation, value) %>%
  arrange(simulation, value) %>%
  mutate(n = 1) %>%
  spread(value, value) 

c[is.na(c)] <- ''

c %>%
  mutate(combo = paste0(James, Jordan, Lester, Luke)) %>%
  group_by(combo) %>%
  summarise(
    n=n()
  ) %>%
  mutate(rate = round(n/10000*100)) %>%
  arrange(desc(rate))


pos_smy <- results %>%
  group_by(pos) %>%
  summarise(
    W_MAX = max(cumul_win),
    W_MED = median(cumul_win),
    W_MIN = min(cumul_win)
  ) %>%
  arrange(pos)

pos_smy

results %>%
  filter(cumul_win == 12 & pos == 5)


results %>%
  filter(simulation == 1818)

results %>%
  filter(coach == 'Simon' & pos == 2)


