source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions.R')

library(zoo)

sc <- get_sc(cid,tkn)


## Calc player stats -----------------------------------------------------------
player_raw <- get_player_data(sc, round=sc$var$current_round)

rnds <- max(player_raw$played)

player_data <- player_raw %>%
  mutate(avg_adj = round(((avg*played) + ((rnds-played)*75))/rnds,1))

# Standardise averages
pos_list <- tibble(
  p = c('DEF','MID','RUC','FWD'),
  n = c(5,7,2,5)
) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(weight = round(n/sum_n,3))

scr_data <- tibble()
overall_smy <- tibble()
for(i in 1:nrow(pos_list)) {
  
  p <- pos_list$p[i]
  n <- pos_list$n[i]
  
  pos_data <- player_data[grepl(p,player_data$pos_1)|grepl(p,player_data$pos_2),]
  
  pos_smy <- pos_data %>%
    select(feed_id, avg_adj) %>%
    arrange(desc(avg_adj)) %>%
    top_n(n*8, avg_adj)
  
  mean<- mean(pos_smy$avg_adj)
  sd<- sd(pos_smy$avg_adj)
  
  scr <- pos_data %>%
    mutate(pos_scr = round((avg_adj - mean)/sd,3)) %>%
    select(feed_id, pos_scr) %>%
    mutate(scr_pos = p)
  
  scr_data <- bind_rows(scr_data, scr)
  overall_smy <- bind_rows(overall_smy, pos_smy)
}  

scr_data <- scr_data %>%
  arrange(desc(pos_scr)) %>%
  distinct(feed_id, .keep_all=T)

mean <- mean(overall_smy$avg_adj)
sd <- sd(overall_smy$avg_adj)

player_data2 <- player_data %>%
  mutate(scr = round((avg_adj - mean)/sd,3)) %>%
  left_join(scr_data, by='feed_id') %>%
  left_join(pos_list[,c('p','weight')], by=c('scr_pos'='p')) %>%
  mutate(scr_wgt = pos_scr*weight + scr*(1-weight)) %>%
  arrange(desc(scr_wgt)) %>%
  mutate(rank = row_number()) %>%
  mutate(name=paste0(substr(first_name,1,1),'.',last_name)) %>%
  select(player_id,name,team_abbrev,avg,avg_adj,scr_wgt, rank)


## Calc Draft data -------------------------------------------------------------

draft_raw <- bind_rows(get_sc_data(sc$auth, sc$url$draft))

draft_data <- draft_raw %>%
  mutate(asl_order = c(seq(1:8), rep(c(seq(1:8),rev(seq(1:8))), max(draft_raw$round)))[1:nrow(draft_raw)]) %>%
  arrange(round, asl_order) %>%
  mutate(pick = row_number()) %>%
  left_join(player_data %>% select(user_team_id, coach) %>% distinct(), by='user_team_id') %>%
  select(user_team_id,coach,round, pick, player_id) %>%
  right_join(player_data2, by='player_id') %>%
  arrange(round, pick, rank) %>%
  mutate(roll_rank = rollapply(rank, 8, rank, align='left', fill=NA)) 


draft_table <- draft_data %>%
  select(coach, round, name) %>%
  filter(!is.na(coach)) %>%
  pivot_wider(names_from = coach, values_from = name)


datatable(
  draft_table,
  rownames=FALSE,
  class = 'row-border stripe',
  colnames = c('Rd'='round'),
  options = list(
    filter=FALSE,
    pageLength = FALSE, 
    lengthChange = FALSE,
    paging=FALSE,
    scrollX = FALSE,
    scrollY = FALSE,
    columnDefs = list(list(className = 'dt-center',targets = 0:(ncol(draft_table)-1)))
  )
)

datatable(
  c,
  rownames=FALSE,
  class = 'row-border stripe',
  colnames = c('Player'='last_name', 'Avg'='avg', 'Delta'='coach_diff', 'N'='n'),
  extensions = 'FixedColumns',
  options = list(
    filter=FALSE,
    pageLength = 10, 
    lengthChange = FALSE,
    paging=TRUE,
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 2),
    columnDefs = list(list(className = 'dt-center', targets = 1:(ncol(c)-1)))
  )
) %>% 
  formatStyle(names(c[,c(5:ncol(c))]), 
              backgroundColor = styleInterval(brks, clrs))



  
