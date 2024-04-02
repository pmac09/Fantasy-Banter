# To Do List

# Import Player List
# Import Player Stat History
# Calc 15 game median
# 5 Tiered rankings (Elite >= 110, Premo >= 100, Serviceable, Stream , Waiver )
# Positional Weighting
# Missed game replacement value

## Stat Ideas


library(tidyverse)
library(zoo)
library(highcharter)

source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') # import supercoach authentication variables

sc <- get_sc(cid, tkn)
playerList <- get_sc_data(sc$auth, paste0(sc$url$players,'0'))


#-------------------------------------------------------------------------------

# Defines zscores at the start and only weights change

playerData <- tibble(
  id = unlist(lapply(playerList, function(x) x$id)),
  feed_id = unlist(lapply(playerList, function(x) x$feed_id)),
  first_name = unlist(lapply(playerList, function(x) x$first_name)),
  last_name = unlist(lapply(playerList, function(x) x$last_name)),
  team = unlist(lapply(playerList, function(x) x$team$abbrev)),
  pos1 = unlist(lapply(playerList, function(x) x$positions[[1]]$position)),
  pos2 = unlist(lapply(playerList, function(x) ifelse(length(x$positions)>1,x$positions[[2]]$position,NA))),
  proj = unlist(lapply(playerList, function(x) x$previous_average))
) 

posList <- c(DEF=5,MID=7,RUC=2,FWD=5,BEN=4)
teamCount <- 8
picks <- sum(posList*teamCount)

# posStats <- tibble(pos = names(posList), spots = posList) %>%
#   filter(pos %in% c('DEF','MID','RUC','FWD')) %>%
#   mutate(draft = ceiling(spots/sum(spots)*sum(posList)*teamCount)) %>%
#   mutate(base = draft/sum(draft))

posStats <- tibble(pos = names(posList), spots = posList) %>%
  filter(pos %in% c('DEF','MID','RUC','FWD')) %>%
  mutate(draft = spots*teamCount) %>%
  mutate(base = draft/sum(draft))

draftOrder <- playerData %>%
  mutate(pos = paste0(pos1, ifelse(is.na(pos2), '', paste0('/',pos2)))) %>%
  select(feed_id, pos, proj) %>%
  filter(proj>0) %>%
  mutate(pick = NA)

zPos <- lapply(posStats$pos, function(p){
  
  posPlayers <- draftOrder[grepl(p, draftOrder$pos),]
        
  posSmy <- posPlayers %>% 
    top_n(posStats$draft[posStats$pos==p] ,proj) %>%
    summarise(
      min=min(proj),
      max=max(proj)
    ) 
  
  posStnd <- posPlayers %>%
    cross_join(posSmy) %>%
    mutate(zPos = (proj-min)/(max-min)) %>%
    mutate(pos = p) %>%
    select(feed_id, pos, zPos)
  
  return(posStnd)
  
}) %>%
  bind_rows() %>%
  arrange(desc(zPos)) %>%
  distinct(feed_id, .keep_all=TRUE) %>%
  rename(draft_pos = pos)

draftOrder2 <- draftOrder %>%
  left_join(zPos, by=c('feed_id'))

zTot <- draftOrder2 %>%
  top_n(picks, zPos) %>%
  summarise(
    min=min(proj),
    max=max(proj)
  ) 

draftOrder3 <- draftOrder2 %>%
  cross_join(zTot) %>%
  mutate(zTot = (proj-min)/(max-min)) %>%
  select(-min, -max)

for( i in 1:picks){
  
  drafted <- draftOrder3 %>%
    filter(!is.na(pick)) %>%
    group_by(draft_pos) %>%
    summarise(drafted=n()) 
  
  posStats2 <- posStats %>%
    left_join(drafted, by=c('pos'='draft_pos')) %>%
    mutate(drafted = ifelse(is.na(drafted),0,drafted)) %>%
    rowwise() %>%
    mutate(remaining = max(draft - drafted, 0)) %>% 
    ungroup() %>%
    mutate(pcnt = remaining/(sum(remaining)))
  
  undrafted <- draftOrder3 %>%
    filter(is.na(pick)) %>%
    left_join(posStats2[,c('pos','pcnt','base')], by=c('draft_pos'='pos')) %>%
    mutate(zscore = zPos * pcnt + zTot * (1-pcnt)) %>%
    mutate(zscore = ifelse(pcnt == 0, zPos, zscore)) %>%
    arrange(desc(zscore)) 
  
  draftOrder3$pick[draftOrder3$feed_id == undrafted$feed_id[1]] <- i
  
}

undrafted <- draftOrder3 %>%
  filter(is.na(pick)) %>%
  left_join(posStats2[,c('pos','base')], by=c('draft_pos'='pos')) %>%
  mutate(zscore = zPos * base + zTot * (1-base)) %>%
  arrange(desc(zscore)) %>%
  mutate(pick = row_number() + sum(!is.na(draftOrder3$pick)))

draftOrder_final <- draftOrder3 %>%
  filter(!is.na(pick)) %>%
  select(feed_id, pick, draft_pos)

# CREATE NOTES
# 1. Cleanse Name
# 2. Default Ordering
# 3. Selection Simulation

notes1 <- playerData %>%
  select(feed_id, first_name, last_name, team, pos1, pos2, proj) %>%
  mutate(name = paste0(substr(first_name,1,1),'.',last_name)) %>%
  group_by(name, team) %>%
  mutate(n = n()) %>%
  mutate(name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), name)) %>%
  ungroup() %>%
  select(-n, -first_name, -last_name) %>%
  pivot_longer(cols=contains('pos'), names_to='pos') %>%
  filter(!is.na(value)) %>%
  select(-pos) %>%
  pivot_wider(names_from='value', values_from = 'name') %>%
  select(feed_id, DEF, MID, RUC, FWD, team, proj)


notes2 <- draftOrder_final %>%
  arrange(pick) %>%
  mutate(round = ceiling(pick/teamCount)) %>%
  group_by(draft_pos) %>%
  mutate(rank = paste0(substr(draft_pos,1,1),row_number())) %>%
  ungroup() %>%
  select(feed_id, round, pick, rank)


notes_final <- notes2 %>%
  left_join(notes1, by=c('feed_id'))

write.csv(notes_final, './scripts/021 Draft Prep/2024/notes.csv', na='', row.names=FALSE)



