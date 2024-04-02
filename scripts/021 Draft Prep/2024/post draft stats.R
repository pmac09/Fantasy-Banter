source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') # import supercoach authentication variables
source('/Users/paulmcgrath/Github/Fantasy-Draft-Assistant/shiny/functions/firebase_functions.R') # import supercoach authentication variables


firebaseDownload <- function(projectURL, path = NULL){
  data <- suppressWarnings(download(projectURL, paste0('/Fantasy-Draft-Assistant/',path)))
  return(data)
} # Download data from firebase location

data <- firebaseDownload(projectURL, 'data/kmJ6QLXZy8wv/')


draftTable <- bind_rows(data$draft_table) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  left_join(ff_players, by=c('player_id'='feed_id'))


age <- draftTable %>%
  mutate(age = year(Sys.Date()) - year(dob)) 

age$coach <- factor(age$coach, levels=c("Richo", 
                                        "Garter", 
                                        "C", "B"))

age %>%
  group_by(coach) %>%
  filter(!is.na(age)) %>%
  summarise(
    age = median(age)
  ) %>%
  arrange(age)

boxplot(age ~ coach, data=new_order)


x <- draftTable %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(playerList, by=c('player_id'='feed_id'))


x %>%
  group_by(coach) %>%
  summarise(
    games=mean(career_games, na.rm=T)
  ) %>%
  arrange(desc(games))


x %>%
  group_by(coach) %>%
  summarise(
    sc=sum(avg, na.rm=T)
  ) %>%
  arrange(desc(sc))


library("RColorBrewer")

brewer.pal(n = 8, name = "Set1")

x <- draftTable %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(dd_data, by=c('player_id'='feed_id')) 

plot(x$pick, 
     x$adp_dd, 
     col = case_when(x$coach == 'Richo' ~ brewer.pal(n = 8, name = "Set1")[1],
                     x$coach == 'Kappaz' ~ brewer.pal(n = 8, name = "Set1")[2],
                     x$coach == 'Chief'  ~ brewer.pal(n = 8, name = "Set1")[3],
                     x$coach == 'Lester'  ~ brewer.pal(n = 8, name = "Set1")[4],
                     x$coach == 'Pmac'   ~ brewer.pal(n = 8, name = "Set1")[5],
                     x$coach == 'Garter'  ~ brewer.pal(n = 8, name = "Set1")[6],
                     x$coach == 'Melons'  ~ brewer.pal(n = 8, name = "Set1")[7],
                     x$coach == 'SPOON'   ~ brewer.pal(n = 8, name = "Set1")[8]),
     main="ASL to ADP",
     xlab="ASL Draft 2024", 
     ylab="Average Draft Position (Draft Doctors)", 
     pch=20)                                    # Plot the data points

legend("topleft",   # Change the position as per your preference
       legend = c('Richo','Kappaz','Chief','Lester', 'Pmac','Garter','Melons','SPOON'),  # Assumes base$pos is categorical
       col = brewer.pal(n = 8, name = "Set1"),  # Colors corresponding to the categories
       pch = 20,  # The same point shape used in the plot
       title = "Coach")  # Title for the legend


names(x) 

y <- x %>%
  select(first_name, last_name, team, pick, coach, adp_dd) %>%
  mutate(adp_diff = pick - adp_dd) %>%
  arrange(adp_diff)

