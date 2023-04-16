################################################################################
# Supercoach Draft Preparation - Data setup

# 1. Update Supercoach Token
#   - Incognito window: log into supercoach with inspect element
#   - Filter for XHR and search for access_token
#   - Copy ENTIRE token which is in the request payload (towards the bottom)
#   - Paste into secrets.R file

################################################################################

# Import common functions
source('./functions/secrets.R')
source('./functions/supercoach_functions.R')

# BASE LIST --------------------------------------------------------------------
sc <- get_sc(cid, tkn)

year <- year(Sys.Date())
url <- paste0('https://supercoach.heraldsun.com.au/',year,'/api/afl/draft/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&round=0')

# need to get ADP in here somehow

sc_data <- get_sc_data(sc$auth, url)
player_list <- lapply(sc_data, unlist)
player_list <- bind_rows(lapply(player_list, as.data.frame.list))

player_list <- tibble(
  feed_id     = as.numeric(player_list$feed_id),
  #player_id   = as.numeric(player_list$id),
  first_name  = player_list$first_name,
  last_name   = player_list$last_name,
  team        = player_list$team.abbrev,
  pos         = trimws(paste(player_list$positions.position, ifelse(is.na(player_list$positions.position.1),"",player_list$positions.position.1))),
  pos1        = player_list$positions.position,
  pos2        = player_list$positions.position.1,
  games       = as.numeric(player_list$previous_games),
  total       = as.numeric(player_list$previous_total),
  average     = as.numeric(player_list$previous_average),
  price       = as.numeric(player_list$player_stats.price)
) %>%
  arrange(desc(price), desc(total), desc(average))

player_info <- fread('http://www.fanfooty.com.au/resource/player.php')
colnames(player_info) <- c(
  'ff_id', 
  'feed_id', 
  'first_name', 
  'last_name', 
  'team', 
  'status', 
  'number', 
  'dob', 
  'height', 
  'weight', 
  'state', 
  'recruit', 
  'career_games', 
  'goals'
)

player_data <- player_list %>%
  mutate(full_name = paste0(first_name,' ',last_name)) %>%
  mutate(player_name = paste0(substr(first_name,1,1), '.',
                              ifelse(is.na(str_extract(first_name, '.\\.')), '', str_extract(first_name, '.\\.')),
                              last_name)) %>%
  left_join(player_info[,c('feed_id', 'number', 'career_games', 'dob')], by='feed_id') %>%
  mutate(age = year-year(dob)) %>%
  mutate(priced_avg = round(price/5474,1)) %>%
  mutate(def = grepl('DEF',pos)) %>%
  mutate(mid = grepl('MID',pos1) & is.na(pos2)) %>%
  mutate(ruc = grepl('RUC',pos)) %>%
  mutate(fwd = grepl('FWD',pos))

# Standardise averages
pos_list <- tibble(
  p = c('DEF','MID','RUC','FWD'),
  n = c(5+1,7+2,2+0,5+1)
) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(weight = round(n/sum_n,3))

scr_data <- tibble()
overall_smy <- tibble()
for(i in 1:nrow(pos_list)) {
  
  p <- pos_list$p[i]
  n <- pos_list$n[i]
  
  pos_data <- player_data[player_data[[tolower(p)]],] 
  
  pos_smy <- pos_data %>%
    filter(games >=3) %>%
    arrange(desc(priced_avg)) %>%
    top_n(n*8, priced_avg) %>%
    select(feed_id, priced_avg)
  
  mean<- mean(pos_smy$priced_avg)
  sd<- sd(pos_smy$priced_avg)
    
  scr <- pos_data %>%
    mutate(pos_scr = round((priced_avg - mean)/sd,3)) %>%
    select(feed_id, pos_scr) %>%
    mutate(scr_pos = p)
  
  scr_data <- bind_rows(scr_data, scr)
  overall_smy <- bind_rows(overall_smy, pos_smy)
}

scr <- scr_data %>%
  arrange(feed_id, desc(pos_scr)) %>%
  distinct(feed_id, .keep_all=T)

mean <- mean(overall_smy$priced_avg)
sd <- sd(overall_smy$priced_avg)

player_data2 <- player_data %>%
  mutate(scr = round((priced_avg - mean)/sd,3)) %>%
  left_join(scr, by='feed_id') %>%
  left_join(pos_list[,c('p','weight')], by=c('scr_pos'='p')) %>%
  mutate(scr_wgt = pos_scr*weight + scr*(1-weight)) %>%
  arrange(desc(scr_wgt))

player_data3 <- player_data2 %>%
  left_join(game_data2, by='feed_id')

write.csv(player_data3,'./analysis/2022/2022 Draft Prep/player_list.csv', row.names = F, na='')













# Ordering Rules ---------------------------------------------------------------
# here is where i will put my methods for ordering creating a projected avg

player_proj <- player_list %>%
  mutate(proj = average)


# Positional Scarcity ----------------------------------------------------------
# Normalise projections based on positional scarcity to create 1 list

positions <- c('DEF','MID','RUC','FWD')
positions_n <- c(5+1,7+2,2+0,5+1) # allocated bench spots based on team %
players_n <- 8

for (i in 1:length(positions)) {
  
  pos_list <- player_proj %>%
    filter(grepl(positions[i],pos)) %>%
    arrange(desc(proj)) 
  
  mean <- pos_list %>%
    top_n(positions_n[i]*players_n) %>%
    summarise(
      mean(proj, rm.na=T)
    )
  
  
  %>%
    mean(proj, rm.na=T)
  
  
}








player_data <- player_data %>%
  select(-round, 
         -projected_points,
         -points,
         -avg,
         -avg3,
         -avg5)


filepath <- './analysis/2022/2022 Draft Prep/'
write.csv(player_data, paste0(filepath,'player_list.csv'))


library(fitzRoy)

data_2021 <- fetch_player_stats_fryzigg(2022)
data_2020 <- fetch_player_stats_fryzigg(2020)
data_2019 <- fetch_player_stats_fryzigg(2019)


