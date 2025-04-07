options(stringsAsFactors = FALSE)
library(tidyverse)
library(fireData)
library(shiny)

pongdash <- paste0('../Beer-Pong-Dashboard/Dashboard/')

source(paste0(pongdash,'functions/secret.R'))
source(paste0(pongdash,'functions/tournament_functions.R'))
source(paste0(pongdash,'functions/firebase_functions.R'))
source(paste0(pongdash,'server/serverDashboard.R'))
source(paste0(pongdash,'server/serverSettings.R'))

vTournamentList <- tournamentList(projectURL)
vTournamentList

tourns <- c(
  'ASL-Draft-2019',
  'ASL_DRAFT_2020',
  'ASL_Draft_2021',
  'ASL_DRAFT_2022_dubs',
  'ASL_DRAFT_2023',
  'ASL_DRAFT_2024',
  'ASL_DRAFT_2025'
)

history <- tibble()
raw <- tibble()
for (i in 1:length(tourns)){
  
  tName <- tourns[i]
  gList <- gameList(projectURL, tName)
  
  data <- leagueData(gList, tName) 
  
  x <-unique(data$GAME)
  
  data2 <- data %>%
    mutate(MATCH = match(GAME, x)) %>%
    mutate(YEAR = 2018+i)
  
  history <- bind_rows(data2,history)
  
  r <- rawData(gList, tName) %>%
    mutate(MATCH = match(GAME, x)) %>%
    mutate(YEAR = 2018+i)
  
  raw <- bind_rows(r,raw)
  
}

history2 <- history  %>%
  mutate(PLAYER = ifelse(PLAYER == 'NAKHLA' | PLAYER == 'NAHKLA', 'MATT', PLAYER)) %>%
  mutate(PLAYER = ifelse(PLAYER == 'SPOON' & YEAR == 2021, 'MELONS',PLAYER)) %>%
  mutate(PLAYER = ifelse(PLAYER == 'SPOON' & YEAR == 2020, 'CHIEF',PLAYER))

raw2 <- raw  %>%
  mutate(PLAYER = ifelse(PLAYER == 'NAKHLA' | PLAYER == 'NAHKLA', 'MATT', PLAYER)) %>%
  mutate(PLAYER = ifelse(PLAYER == 'SPOON' & YEAR == 2021, 'MELONS',PLAYER)) %>%
  mutate(PLAYER = ifelse(PLAYER == 'SPOON' & YEAR == 2020, 'CHIEF',PLAYER))

elo <- history2 %>%
  select(YEAR, MATCH, GAME, TEAM, PLAYER, HITS, OVERTHROWS)


elo2 <- as_tibble(elo) %>%
  arrange(YEAR,MATCH) %>%
  rowwise() %>%
  mutate(SCORE = max(HITS - OVERTHROWS, 0)) %>%
  mutate(GAME_ID = paste0(YEAR,"_",GAME)) %>%
  mutate(RATING = 1000) %>%
  select(GAME_ID, TEAM, PLAYER, RATING, SCORE) %>%
  mutate(NEW_RATING = NA) 

#%>% 
#  filter(!PLAYER %in% c('MELONS','GARTER'))


games <- unique(elo2$GAME_ID)

h2h<- tibble()

for (i in 1:length(games)){

game <- games[i]

game_data <- elo2[elo2$GAME_ID == game, c('GAME_ID', 'TEAM', 'PLAYER', 'RATING', 'SCORE', 'NEW_RATING')]

game_data2 <- left_join(game_data, game_data, by=c('GAME_ID'), relationship = "many-to-many") %>%
  filter(PLAYER.x != PLAYER.y) %>%
  filter(TEAM.x != TEAM.y) %>%
  mutate(K = 20) %>%
  mutate(ACTUAL = ifelse(SCORE.x > SCORE.y, 1, ifelse(SCORE.x == SCORE.y, 0.5, 0))) %>%
  mutate(EXPECTED = 1/(1+10^((RATING.y-RATING.x)/400))) %>%
  mutate(Q = 2.2/((ifelse(ACTUAL == 1, SCORE.x, SCORE.y) - ifelse(ACTUAL == 1, SCORE.y, SCORE.x))*0.001+2.2)) %>%
  mutate(MoVM = base::log(abs(SCORE.x - SCORE.y)+1)) %>%
  mutate(change = K * (ACTUAL - EXPECTED) * MoVM) %>%
  mutate(change = round(change,0))

h2h <-bind_rows(h2h, game_data2[,c('GAME_ID','PLAYER.x', 'PLAYER.y', 'change')])

game_data3 <- game_data2 %>%
  group_by(PLAYER.x) %>%
  summarise(
    CHANGE = sum(change),
    .groups = 'drop'
  )

game_data4 <- left_join(game_data, game_data3, by=c('PLAYER' = 'PLAYER.x')) %>%
  mutate(NEW_RATING = RATING + CHANGE) %>%
  select(GAME_ID, PLAYER, NEW_RATING)


elo2 <- left_join(elo2, game_data4, by=c('GAME_ID', 'PLAYER')) %>%
  mutate(NEW_RATING=coalesce(NEW_RATING.x,NEW_RATING.y)) %>%
  select(-NEW_RATING.x,-NEW_RATING.y) %>%
  ungroup()


idx <- max(grep(game, elo2$GAME_ID)) 

if(idx < nrow(elo2)){
  elo2 <- left_join(elo2, game_data4[,c('PLAYER','NEW_RATING')], by=c('PLAYER'))%>%
    mutate(rownum = row_number()) %>%
    mutate(RATING = ifelse(rownum > idx & !is.na(NEW_RATING.y), NEW_RATING.y, RATING)) %>%
    select(-NEW_RATING.y,-rownum) %>%
    rename(NEW_RATING = NEW_RATING.x)
}

}

write.csv(elo2,'./scripts/010 Pong Teams/Pong_ELO.csv')


h2h_summary <- h2h %>%
  group_by(PLAYER.x, PLAYER.y) %>%
  summarise(
    total = sum(change)
  ) %>%
  arrange(desc(total)) %>%
  group_by(PLAYER.x) %>%
  mutate(n= row_number())

print(h2h_summary, n=20)

h2h_summary %>%
  filter(n==7) %>%
  arrange(desc(total))

h2h_summary %>%
  select(-n) %>%
  arrange(PLAYER.y) %>%
  pivot_wider(names_from=PLAYER.y, values_from = total) %>%
  arrange(PLAYER.x)
  

x <- h2h_summary %>%
  filter(PLAYER.x == 'PMAC')



x <- playerData(history2)



history2 %>%
  group_by(PLAYER,YEAR) %>%
  summarise(
    dry_spell = max(DRY_SPELL)
  ) %>%
  arrange(desc(dry_spell))

history2 %>%
  group_by(PLAYER, YEAR) %>%
  summarise(
    FANTASY = max(FANTASY)
  ) %>%
  arrange(desc(FANTASY))

history2 %>%
  group_by(YEAR) %>%
  summarise(
    catches = sum(T_THROWS)
  ) 

history2 %>%
  group_by(YEAR) %>%
  summarise(
    overthrows = sum(OVERTHROWS)
  ) 

#PCNT through the years

x <- history2 %>%
  group_by(YEAR,PLAYER) %>%
  summarise(
    HITS = sum(HITS),
    THROWS = sum(THROWS),
  )

y <- x %>%
  mutate(PCNT = round(HITS/THROWS*100,1)) %>%
  select(YEAR,PLAYER,PCNT) %>%
  pivot_wider(names_from=YEAR,values_from = PCNT)

z <- x %>%
  group_by(YEAR) %>%
  summarise(
    HITS = sum(HITS),
    THROWS = sum(THROWS),
  ) %>%
  mutate(PCNT = round(HITS/THROWS*100,1)) 

# PCNT by GAME type

gameTypePcnt <- history2 %>%
  mutate(TYPE = case_when(substr(GAME,1,1)=='R' ~ 'Round Robin',
                          substr(GAME,1,1)=='I' ~ 'Individuals',
                          TRUE ~ 'Team Finals')) %>%
  group_by(YEAR, TYPE)%>%
  summarise(
    HITS = sum(HITS),
    THROWS = sum(THROWS),
  ) %>%
  mutate(PCNT = round(HITS/THROWS*100,1))  %>%
  select(YEAR,TYPE,PCNT) %>%
  pivot_wider(names_from=YEAR,values_from = PCNT) %>%
  arrange(match(TYPE, c('Round Robin','Team Finals','Individuals')))
  
gameTypePcnt <- history2 %>%
  mutate(TYPE = case_when(substr(GAME,1,1)=='R' ~ 'Round Robin',
                          substr(GAME,1,1)=='I' ~ 'Individuals',
                          TRUE ~ 'Team Finals')) %>%
  filter(YEAR == 2025) %>%
  group_by(PLAYER, TYPE)%>%
  summarise(
    HITS = sum(HITS),
    THROWS = sum(THROWS),
  ) %>%
  mutate(PCNT = round(HITS/THROWS*100,1))  %>%
  select(PLAYER,TYPE,PCNT) %>%
  pivot_wider(names_from=PLAYER,values_from = PCNT) %>%
  arrange(match(TYPE, c('Round Robin','Team Finals','Individuals')))



#hit percentage per cup

perCup <- raw2 %>%
  filter(YEAR == 2025) %>%
  select(PLAYER, TEAM_SCORE_CUMUL, TARGET, SCORE_CHANGE) %>%
  mutate(CUPS_REM = TARGET - TEAM_SCORE_CUMUL)

pcT <- perCup %>%
  group_by(PLAYER, CUPS_REM) %>%
  summarise(
    THROWS = n()
  )

pcH <-perCup %>%
  group_by(PLAYER, CUPS_REM) %>%
  summarise(
    HITS = sum(SCORE_CHANGE)
  ) %>%
  mutate(CUPS_REM = CUPS_REM+1)

perCupFinal <- pcT %>%
  left_join(pcH, by=c('PLAYER','CUPS_REM')) %>%
  filter(!is.na(HITS)) %>%
  mutate(PCNT = round(HITS/THROWS*100)) %>%
  select(PLAYER,PCNT,CUPS_REM) %>%
  pivot_wider(names_from=PLAYER,values_from = PCNT) %>%
  arrange(desc(CUPS_REM))

perCupFinal_smy <- pcT %>%
  left_join(pcH, by=c('PLAYER','CUPS_REM')) %>%
  filter(!is.na(HITS)) %>%
  group_by(CUPS_REM) %>%
  summarise(
    HITS = sum(HITS),
    THROWS = sum(THROWS)
  ) %>%
  mutate(PCNT = round(HITS/THROWS*100,1)) %>%
  select(PCNT,CUPS_REM) %>%
  arrange(desc(CUPS_REM))



perCupFinal <- pcT %>%
  left_join(pcH, by=c('PLAYER','CUPS_REM')) %>%
  filter(!is.na(HITS)) %>%
  select(PLAYER,HITS,CUPS_REM) %>%
  pivot_wider(names_from=PLAYER,values_from = HITS) %>%
  arrange(desc(CUPS_REM))



margin <- history2 %>%
  mutate(MARGIN = CUPS_FOR - CUPS_AGAINST) %>%
  group_by(PLAYER, MARGIN) %>%
  summarise(
    n = n()
  ) %>%
  arrange(MARGIN) %>%
  pivot_wider(names_from = MARGIN, values_from = n)
  
margin <- history2 %>%
  mutate(MARGIN = CUPS_FOR - CUPS_AGAINST) %>%
  mutate(MARGIN_TYPE = ifelse(MARGIN<0,"LOSS","WIN")) %>%
  group_by(PLAYER,MARGIN_TYPE) %>%
  summarise(
    avg = round(mean(MARGIN),1)
  ) %>%
  pivot_wider(names_from=MARGIN_TYPE, values_from = avg)





history2 %>%
  group_by(PLAYER) %>%
  summarise(
    hits = sum(HITS),
    games = n()
  ) %>%
  mutate(cpg = round(hits/games,3)) %>%
  arrange(desc(cpg)) %>%
  select(-hits, -games)


history2 %>%
  group_by(PLAYER, YEAR) %>%
  summarise(
    hits = sum(HITS) + sum(T_HITS)
  ) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from=YEAR, values_from=hits)


history2 %>%
  group_by(PLAYER, YEAR) %>%
  summarise(
    TRICKS= sum(T_HITS)
  ) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from=YEAR, values_from=TRICKS)

history2 %>%
  group_by( YEAR) %>%
  summarise(
    TRICKS= sum(T_HITS)
  ) %>%
  arrange(YEAR) 


history2 %>%
  group_by(PLAYER) %>%
  summarise(
    overthrows = sum(OVERTHROWS),
    games = n()
  ) %>%
  mutate(opg = round(overthrows/games,3)) %>%
  arrange(desc(opg))


history2 %>%
  group_by(PLAYER, YEAR) %>%
  summarise(
    overthrows = sum(OVERTHROWS)
  ) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from=YEAR, values_from=overthrows)


len <- history2 %>%
  group_by(YEAR, GAME) %>%
  summarise(
    throws = sum(THROWS)
  ) %>%
  filter(substr(GAME,1,1) !='I')

boxplot(throws ~ YEAR, data=len)


x <- raw %>%
  group_by(YEAR, GAME) %>%
  mutate(dry = cumsum(SCORE_CHANGE)) %>%
  group_by(YEAR, GAME, dry) %>%
  summarise(
    n=n()
  )

x <- history2 %>%
  group_by(YEAR,GAME,TEAM) %>%
  summarise(
    h = sum(HITS),
    t = sum(THROWS)
  ) %>%
  filter(YEAR == 2025) %>%
  filter(substr(GAME,1,1)=='R')

y <-  x %>%
   left_join(x, by=c('GAME'), relationship = "many-to-many") %>%
  filter(TEAM.x != TEAM.y) %>% 
  group_by(GAME,TEAM.x) %>%
  summarise(
    h = sum(h.y),
    t = sum(t.y)
  ) %>%
  mutate(pcnt = round(h/t*100,1))


x <- history2 %>%
  select(YEAR,GAME,TEAM,PLAYER,HITS,THROWS) %>%
  left_join(history2[,c('YEAR','GAME','TEAM')], by=c('YEAR','GAME'), relationship = "many-to-many") %>%
  filter(TEAM.x != TEAM.y) %>%
  mutate(pcnt = round(HITS/THROWS*100,1)) %>%
           arrange(desc(pcnt)) %>%
  distinct_all()

x <- history2 %>%
  select(YEAR,GAME,TEAM,PLAYER,HITS,THROWS) %>%
  mutate(pcnt = round(HITS/THROWS*100,1)) %>%
  arrange(pcnt)

x %>%
  group_by(PLAYER) %>%
  top_n(1,-pcnt)
  