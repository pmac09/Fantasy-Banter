## SETUP ----
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')
path <- './scripts/999 Player Analysis'

library(highcharter)

## FUNCTIONS ----
numeric_col <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}


## START ----

sc <- get_sc(cid,tkn)

# Raw Player List data
url <- 'https://supercoach.heraldsun.com.au/2023/api/afl/draft/v1/players-cf?embed=positions'
player_list_raw <- get_sc_data(sc$auth, url)

# Cleanse position
player_list_raw1 <- lapply(player_list_raw, function(x) {
  pos <- paste(sort(unlist(lapply(x$positions, function(x) x$position))), collapse = ' ')
  x$position <- pos
  x$positions <- NULL
  return(x)
})

# Create PLayer List
player_list <- bind_rows(lapply(player_list_raw1, function(x) data.frame(as.list(unlist(x))))) %>%
  mutate_if(numeric_col,as.numeric) %>%
  mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
  filter(active == TRUE) %>%
  select(
    feed_id,
    id,
    first_name,
    last_name,
    player_name,
    position,
    team.id,
    team.name,
    team.abbrev
  )


# Extract Player Data
n_players <- nrow(player_list)
progress <- round(seq(1,n_players,n_players/100),0)


stats_current_master <- tibble()
stats_history_master <- tibble()
for (i in 1:10){
  
  if(i %in% progress) print(round(i/n_players*100,0))
  
  player_id <- player_list$id[i]
  
  # Current Stats
  url <- paste0('https://supercoach.heraldsun.com.au/2023/api/afl/draft/v1/players/', player_id,'?embed=player_stats')
  stats_current_raw <- get_sc_data(sc$auth, url)$player_stats
  
  stats_current <- bind_rows(lapply(stats_current_raw, function(x) data.frame(as.list(unlist(x))))) %>%
    mutate_if(numeric_col,as.numeric)
  
  stats_current_master <- bind_rows(stats_current_master, stats_current)
  
  
  # History Stats
  url <- paste0('https://supercoach.heraldsun.com.au/2023/api/afl/draft/v1/completeStatspack?player_id=', player_id)
  stats_history_raw <- get_sc_data(sc$auth, url)$playerStats
  
  stats_history <- bind_rows(lapply(stats_history_raw, function(x) data.frame(as.list(unlist(x))))) %>%
    mutate_if(numeric_col,as.numeric)
  
  stats_history_master <- bind_rows(stats_history_master, stats_history)
  
}


stat_cols <- c(
  'player_id',
  'season',
  'team.abbrev',
  'round',
  'games',
  'points'
)

player_stats <- stats_current_master %>%
  mutate(season = year(Sys.Date())) %>%
  left_join(player_list, by=c('player_id'='id')) %>%
  select(all_of(stat_cols)) %>%
  bind_rows(stats_history_master[,stat_cols]) %>%
  arrange(player_id, desc(season), desc(round))


player_stats1 <- player_list %>%
  select(-team.id, -team.name) %>%
  rename(team_abbrev = team.abbrev) %>%
  inner_join(player_stats, by=c('id'='player_id'))



hc_data <- player_stats1 %>%
  filter(id == 18) %>%
  mutate(xaxis = as.numeric(paste0(substr(season,3,4),'.',formatC(round, width = 2, format = "d", flag = "0")))) %>%
  arrange(xaxis)
  

highchart() %>%
  hc_chart(type='column') %>%
  hc_xAxis(
    categories = hc_data$xaxis,
    scrollbar = list(enabled=TRUE),
    min = nrow(hc_data)-31,
    max = nrow(hc_data)-1
  ) %>%
  hc_add_series(data=hc_data$points) 


