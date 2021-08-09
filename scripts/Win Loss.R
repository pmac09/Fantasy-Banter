options(scipen = 999)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

sc <- get_sc(cid, tkn)
league_data <- get_league_data(sc)

x <- league_data %>%
  mutate(scores = cut(team_score, 
                      breaks = c(-Inf, 1500, 1600, 1700, 1800, 1900, Inf),
                      labels = c('<1500',paste0('>',seq(1500,1900,100))))) %>%
  group_by(coach, scores) %>%
  summarise(
    n=n()
  ) %>%
  spread(coach, n)


y <- league_data %>%
  filter(!is.na(team_score)) %>%
  mutate(scores = cut(team_score, 
                      breaks = c(-Inf, 1500, 1600, 1700, 1800, Inf),
                      labels = c('<1500',paste0('>',seq(1500,1800,100))))) %>%
  mutate(win = ifelse(win==1,'W','L')) %>%
  group_by(coach, scores, win) %>%
  summarise(
    n=n()
  ) %>%
  spread(win, n)

y[is.na(y)] <- 0  

z <- y %>%
  mutate( 'W_L' = paste0(W,'/',L)) %>%
  select(coach, scores, W_L) %>%
  spread(coach, W_L)


a <- league_data %>%
  filter(!is.na(team_score)) %>%
  mutate(scores = cut(team_score, 
                      breaks = c(-Inf, 1500, 1600, 1700, 1800, Inf),
                      labels = c('<1500',paste0('>',seq(1500,1800,100))))) %>%
  mutate(win = ifelse(win==1,'W','L')) %>%
  group_by(scores, win) %>%
  summarise(
    n=n()
  ) %>%
  spread(win, n) %>%
  mutate(L = ifelse(is.na(L), 0, L)) %>%
  mutate( 'W_L' = paste0(W,'/',L)) %>%
  mutate(pcnt = round(W/(W+L)*100,1)) %>%
  select(scores, W_L, pcnt)

b <- z %>%
  left_join(a, by=c('scores'))

b

