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
  mutate(draft = ceiling(spots/sum(spots)*sum(posList)*teamCount)) %>%
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
      mean=mean(proj),
      sd=sd(proj)
    ) 
  
  posStnd <- posPlayers %>%
    cross_join(posSmy) %>%
    mutate(zPos = (proj-mean)/sd) %>%
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
    mean=mean(proj),
    sd=sd(proj)
  ) 

draftOrder3 <- draftOrder2 %>%
  cross_join(zTot) %>%
  mutate(zTot = (proj-mean)/sd) %>%
  select(-mean, -sd)

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
    mutate(pcnt = remaining/sum(remaining))

  undrafted <- draftOrder3 %>%
    filter(is.na(pick)) %>%
    left_join(posStats2[,c('pos','pcnt','base')], by=c('draft_pos'='pos')) %>%
    mutate(zscore = zPos * pcnt + zTot * (1-pcnt)) %>%
    arrange(desc(zscore)) %>%
    filter(pcnt > 0)
  
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





#


sc <- get_sc(cid, tkn)
playerList <- get_sc_data(sc$auth, paste0(sc$url$players,'0'))

adp <- lapply(playerList, function(pl){
  url <- paste0('https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/players/',pl$id,'?embed=notes,odds,player_stats,player_match_stats,positions,trades')
  get_sc_data(sc$auth, url)
})


url <- paste0('https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/players/%s?embed=notes,odds,player_stats,player_match_stats,positions,trades')

sprintf(url, 123)


a <-lapply(adp, function(x) x$player_stats[[1]]$adp)
a[sapply(a, is.null)] <- NA

b <-lapply(adp, function(x) x$player_stats[[1]]$adp_rank)
b[sapply(b, is.null)] <- NA



adpData <- tibble(
  id = unlist(lapply(adp, function(x) x$id)),
  feed_id = unlist(lapply(adp, function(x) x$feed_id)),
  first_name = unlist(lapply(adp, function(x) x$first_name)),
  last_name = unlist(lapply(adp, function(x) x$last_name)),
  team = unlist(lapply(adp, function(x) x$team$abbrev)),
  pos1 = unlist(lapply(adp, function(x) x$positions[[1]]$position)),
  pos2 = unlist(lapply(adp, function(x) ifelse(length(x$positions)>1,x$positions[[2]]$position,NA))),
  proj = unlist(lapply(adp, function(x) x$previous_average)),
  adp = unlist(a),
  rank = unlist(b)
) 

write.csv(adpData, paste0('./scripts/021 Draft Prep/2024/adp ',Sys.Date(),'.csv'), na='', row.names=FALSE)



base <- adpData %>%
  filter(adp <= 200) %>%
  filter(pos1 == 'MID' & is.na(pos2)) %>%
  filter(proj > 0)

x <- base$adp
y <- base$proj


# Define the degree of the polynomial
degree <- 2  # You can adjust this value as needed

# Fit the polynomial regression model
model <- lm(y ~ poly(x, degree, raw = TRUE))

# Print summary of the model
summary(model)

# Generate predicted Y values using the fitted model
predicted <- predict(model)

# Plot the data points
plot(x, y, main="Scatterplot with Fitted Polynomial Regression",
     xlab="X", ylab="Y", pch=19)

# Add the fitted model curve to the plot
lines(sort(x), predicted[order(x)], lwd=2)


#-------


# Fit the polynomial regression model
model <- lm(y ~ poly(x, degree, raw = TRUE))

# Get the residuals
residuals <- residuals(model)

# Calculate the mean and standard deviation of residuals
residuals_mean <- mean(residuals)
residuals_sd <- sd(residuals)

# Set a threshold for outliers (e.g., 2 standard deviations)
threshold <- 2 * residuals_sd

# Identify outliers based on the threshold
outliers <- which(abs(residuals) > threshold)

# Print the indices of identified outliers
print(outliers)

# Remove outliers from the dataset
x_clean <- x[-outliers]
y_clean <- y[-outliers]

# Fit the polynomial regression model to the cleaned dataset
model_clean <- lm(y_clean ~ poly(x_clean, degree, raw = TRUE))

# Print summary of the cleaned model
summary(model_clean)

# Generate predicted Y values using the cleaned model
predicted_clean <- predict(model_clean)

# Plot the data points
plot(x_clean, y_clean, main="Scatterplot with Fitted Polynomial Regression (Outliers Removed)",
     xlab="X", ylab="Y", pch=19)

# Add the fitted model curve to the plot
lines(sort(x_clean), predicted_clean[order(x_clean)], lwd=2)

plot(x, y, main="Scatterplot with Fitted Polynomial Regression",
     xlab="X", ylab="Y", pch=19)


#--------

# Define new x values for prediction
new_x <- c(115,110,105,100,95,90,85,80) # Example new x values

# Example new x values

# Predict the y values for the new x values using the cleaned model
predicted_y <- predict(model_clean, newdata = data.frame(y_clean = new_x))

# Print the predicted y values
print(predicted_y)
