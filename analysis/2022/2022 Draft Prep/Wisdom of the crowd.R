
library(rjson)


file <- './analysis/2022/2022 Draft Prep/classic.json'
json_data <- fromJSON(paste(readLines(file), collapse=""))


player_list <- lapply(json_data, unlist)
player_list <- bind_rows(lapply(player_list, as.data.frame.list))


players <- tibble(
  feed_id     = as.numeric(player_list$feed_id),
  first_name  = player_list$first_name,
  last_name   = player_list$last_name,
  team        = player_list$team.abbrev,
  pos         = paste0(player_list$positions.position,' ', ifelse(is.na(player_list$positions.position.1),'',player_list$positions.position.1)),
  price       = as.numeric(player_list$player_stats.price),
  own         = as.numeric(player_list$player_stats.own),
  status      = player_list$injury_suspension_status
) %>%
  #filter(status == 'PlayingNextRound') %>%
  arrange(desc(own)) %>%
  mutate(DEF = grepl('DEF',pos)) %>%
  mutate(MID = grepl('MID',pos)) %>%
  mutate(RUC = grepl('RUC',pos)) %>%
  mutate(FWD = grepl('FWD',pos)) %>%
  group_by(DEF) %>%
  mutate(DEF_n = ifelse(DEF,row_number(),NA)) %>%
  ungroup() %>%
  group_by(MID) %>%
  mutate(MID_n = ifelse(MID,row_number(),NA)) %>%
  ungroup() %>%
  group_by(RUC) %>%
  mutate(RUC_n = ifelse(RUC,row_number(),NA)) %>%
  ungroup() %>%
  group_by(FWD) %>%
  mutate(FWD_n = ifelse(FWD,row_number(),NA)) %>%
  ungroup() %>%
  select(-DEF,-MID,-RUC,-FWD) %>%
  rowwise() %>%
  mutate(min = min(DEF_n,MID_n,RUC_n,FWD_n, na.rm=T)) %>%
  ungroup() 

players[,c('DEF_n','MID_n','RUC_n','FWD_n')][is.na(players[,c('DEF_n','MID_n','RUC_n','FWD_n')])] <- 999

players2 <- players %>%
  mutate(POS = ifelse(min==DEF_n,'DEF',ifelse(min==MID_n,'MID',ifelse(min==RUC_n,'RUC','FWD')))) %>%
  group_by(POS) %>%
  mutate(n = row_number())

write.csv(players2, 'classic.csv',na='', row.names = F)

gen.next.cbn <- function(cbn, n){
  ## Generates the combination that follows the one provided as input
  cbn.bin      <- rep(0, n)
  cbn.bin[cbn] <- 1
  if (tail(cbn.bin, 1) == 0){
    ind <- tail(which(cbn.bin == 1), 1)
    cbn.bin[c(ind, ind+1)] <- c(0, 1)
  }else{
    ind <- 1 + tail(which(diff(cbn.bin) == -1), 1)
    nb  <- sum(cbn.bin[-c(1:ind)] == 1)
    cbn.bin[c(ind-1, (n-nb+1):n)] <- 0
    cbn.bin[ind:(ind+nb)]         <- 1
  }
  cbn <- which(cbn.bin == 1)
}



positions <- c('DEF','MID','RUC','FWD')
pos_results <- list()

for(p in positions){
  
  # Player Pool
  pool <- players %>%
    filter(grepl(p, pos)) %>%
    slice(1:15)
  
  # Total Players
  t <- nrow(pool)
  
  # Number of selections
  n <- ifelse(p %in% c('DEF','FWD'),8,ifelse(p=='MID',11,3))
  
  # Total possible combos
  combos <- choose(t, n)
  
  # Results container
  best_combos <- tibble()
  c <- 0
  
  # Loop through each combo
  for(i in 1:combos){
    
    # Get next combination
    if (i == 1){
      cbn <- 1:n
    }else{
      cbn <- gen.next.cbn(cbn, t)
    }
    
    cbn_str <- paste0(cbn, collapse = "|")
    
    new_combo <- pool[cbn,] %>%
      summarise(
        price = sum(price),
        own = sum(own)
      ) %>%
      mutate(cbn = cbn_str)
    
    best_combos <- bind_rows(best_combos, new_combo)
    
    if(cbn[1] != c){
      c <- cbn[1]
      print(paste0(format(Sys.time(), '%H:%M:%S'),' - ',p, ': ', c,'/',t-(n-1)))
    }
    
  }
  
  pos_results[[p]] <- best_combos
  
}


best_team <- tibble()

for(d in 1:nrow(pos_results$DEF)){
  
  print(paste0(Sys.time(),': ',d))
  
  for(m in 1:nrow(pos_results$MID)){
    
    for(r in 1:nrow(pos_results$RUC)){
      
      for(f in 1:nrow(pos_results$FWD)){
      
        df <- pos_results$DEF[d,] %>%
          bind_rows(pos_results$MID[m,]) %>%
          bind_rows(pos_results$RUC[r,]) %>%
          bind_rows(pos_results$FWD[f,]) %>%
          summarise(
            price = sum(price),
            own = sum(own)
          ) %>%
          mutate(cbn = paste0(d,'|',m,'|',r,'|',f)) 
        
        best_team <- bind_rows(best_team, df)
      }
      
      best_team <- best_team %>%
        filter(price < 10000000)
    }
  }
}
    







