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

x <- playerList[[1]]

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

# playerData: dataframe with columns : c(id, pos, proj)
posList <- c(DEF=5,MID=7,RUC=2,FWD=5,BEN=4)
teamCount <- 8

posStats <- tibble(pos = names(posList), spots = posList) %>%
  filter(pos %in% c('DEF','MID','RUC','FWD')) %>%
  mutate(draft = ceiling(spots/sum(spots)*sum(posList)*teamCount)) %>%
  mutate(base = draft/sum(draft))


draftOrder <- playerData %>%
  mutate(rank = NA) 

zscores <- lapply(posStats$pos, function(p){
  
  posPlayers <- draftOrder %>% 
    filter(pos1 == p | pos2 == p) %>%
    mutate(pos = p)
  
  posSmy <- posPlayers %>% 
    top_n(posStats$draft[posStats$pos==p], proj) %>%
    summarise(
      mean=mean(proj),
      sd=sd(proj)
    ) 
  
  posStnd <- posPlayers %>%
    cross_join(posSmy) %>%
    mutate(zPos = (proj-mean)/sd) %>%
    select(feed_id, pos, zPos)
  
  return(posStnd)
  
})
zscores2 <- bind_rows(zscores) %>%
  arrange(desc(zPos)) %>%
  distinct(feed_id, .keep_all=TRUE)

draftOrder2 <- draftOrder %>%
  left_join(zscores2, by=c('feed_id'))

zTotal <- draftOrder2 %>%
  top_n(sum(posStats$draft), zPos) %>%
  summarise(
    mean=mean(proj),
    sd=sd(proj)
  ) 

draftOrder3 <- draftOrder2 %>%
  cross_join(zTotal) %>%
  mutate(zTot = (proj-mean)/sd) %>%
  select(-mean, -sd)

x <- tibble()
#sum(posStats$draft)

for( i in 1:100){
  
  drafted <- draftOrder3 %>%
    filter(!is.na(rank)) %>%
    group_by(pos) %>%
    summarise(drafted=n()) 
  
  posStats2 <- posStats %>%
    left_join(drafted, by=c('pos')) %>%
    mutate(drafted = ifelse(is.na(drafted),0,drafted)) %>%
    rowwise() %>%
    mutate(remaining = max(draft - drafted, 0)) %>% 
    ungroup() %>%
    mutate(pcnt = remaining/(sum(remaining)))
    
  undrafted <- draftOrder3 %>%
    filter(is.na(rank)) 
  
  undrafted2 <- undrafted %>%
    left_join(posStats2[,c('pos','pcnt')], by=c('pos'))
  
  undrafted3 <- undrafted2 %>%
    mutate(zscore = zPos * pcnt + zTot * (1-pcnt)) %>%
    arrange(desc(zscore)) 
  
  draftOrder3$rank[draftOrder3$feed_id == undrafted3$feed_id[1]] <- i
  
  
  x <- bind_rows(x, 
                 undrafted3[undrafted3$feed_id==290550,c('feed_id','last_name','proj', 'zPos','pcnt', 'zTot', 'zscore')] %>%
                   mutate(pick = i))
  
}

draftOrder4 <- draftOrder3 %>%
  arrange(desc(proj)) %>%
  mutate(raw = row_number()) %>%
  mutate(diff = raw - rank) %>%
  arrange(rank, desc(zPos)) %>%
  group_by(pos) %>%
  mutate(count = row_number())
  

x

y <- draftOrder2 %>%
  filter(pos =='FWD')



