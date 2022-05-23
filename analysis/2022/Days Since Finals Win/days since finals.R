library(tidyverse)
library(janitor)
library(scales)
library(ggplot2)
library(gganimate)
library(gifski)

filepath <- './analysis/2022/Days Since Finals Win/'
setwd(filepath)

# Build data
df <-bind_rows(
  c(coach='PMAC',   last_win='2021-08-22'),
  c(coach='GARTER', last_win='2021-08-15'),
  c(coach='RICHO',  last_win='2020-09-21'),
  c(coach='CHIEF',  last_win='2020-09-14'),
  c(coach='KAPPAZ', last_win='2019-08-25'),
  c(coach='JMERC',  last_win='2017-08-20'),
  c(coach='MELONS', last_win='2016-08-28'),
  c(coach='LESTER', last_win='2014-08-24')
) %>%
  mutate(last_win = as.Date(last_win)) %>%
  add_row(coach='Z',last_win=Sys.Date()) %>%
  mutate(days_since_win = as.numeric(Sys.Date()-last_win)) 


charts <- tibble(chart = seq(0,3000,by=3))

df2<- df %>%
  rename(days = days_since_win) %>%
  full_join(charts, by=character(0)) %>%
  mutate(days = ifelse(days < chart, days, chart)) %>%
  mutate(days_lbl = paste0(" ",round(days))) %>%
  arrange(chart, desc(days), coach) %>%
  group_by(chart)  %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(days=ifelse(coach=='Z', 3000,days))

df2 <- df2 %>% filter(chart == 1000)


staticplot <- ggplot(df2, aes(rank, group=coach, fill=as.factor(coach), color=as.factor(coach))) +
   geom_tile(aes(y=days/2,
                 height = days,
                 width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(coach)), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=days,label = days_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous( labels = scales::comma) +
  scale_x_reverse() +
  guides(color = 'none', fill = 'none') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


anim <- staticplot + transition_states(chart, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GUESS THE STAT')


animate(anim, fps = 20, duration = 15, width = 800, height = 600, 
        renderer = gifski_renderer("ASL_bants.gif"))



#### New Project

library(fitzRoy)
library(tidyverse)
library(janitor)
library(scales)
library(ggplot2)
library(gganimate)
library(av)
library(gifski)


seasons <- c(2013:2021)
fixture <- tibble()
for (szn in seasons){
  
  x <- fetch_fixture(season = szn, comp = "AFLM") %>%
    filter(startsWith(round.name, 'Round')) %>%
    filter(round.roundNumber >= max(round.roundNumber)-1) %>%
    group_by(compSeason.name, round.name, round.roundNumber) %>%
    summarise(round.end = max(utcStartTime),
              .groups='drop')
  
  fixture <- bind_rows(fixture, x)
}

fixture2 <- fixture %>%
  mutate(season = as.numeric(substr(compSeason.name,1,4))) %>%
  rename(round = round.roundNumber) %>%
  mutate(win_date = as.Date(round.end)) %>%
  select(season, round, win_date)


wins <- bind_rows(
  c(season=2013, round=22, coach='CHIEF'),
  c(season=2013, round=22, coach='RICHO'),
  c(season=2013, round=23, coach='CHIEF'),
  c(season=2014, round=22, coach='CHIEF'),
  c(season=2014, round=22, coach='LESTER'),
  c(season=2014, round=23, coach='CHIEF'),
  c(season=2015, round=22, coach='MELONS'),
  c(season=2015, round=22, coach='PMAC'),
  c(season=2015, round=23, coach='MELONS'),
  c(season=2016, round=22, coach='MELONS'),
  c(season=2016, round=22, coach='CHIEF'),
  c(season=2016, round=23, coach='MELONS'),
  c(season=2017, round=22, coach='PMAC'),
  c(season=2017, round=22, coach='JMERC'),
  c(season=2017, round=23, coach='PMAC'),
  c(season=2018, round=22, coach='KAPPAZ'),
  c(season=2018, round=22, coach='CHIEF'),
  c(season=2018, round=23, coach='KAPPAZ'),
  c(season=2019, round=22, coach='KAPPAZ'),
  c(season=2019, round=22, coach='PMAC'),
  c(season=2019, round=23, coach='KAPPAZ'),
  c(season=2020, round=17, coach='RICHO'),
  c(season=2020, round=17, coach='CHIEF'),
  c(season=2020, round=18, coach='RICHO'),
  c(season=2021, round=22, coach='PMAC'),
  c(season=2021, round=22, coach='GARTER'),
  c(season=2021, round=23, coach='PMAC')
) %>%
  mutate(season = as.numeric(season)) %>%
  mutate(round = as.numeric(round)) %>%
  left_join(fixture2, by=c('season','round'))


dates <- tibble(date = seq.Date(as.Date('2013-03-01'),Sys.Date(), by='month'))

coach <- tibble(coach = c('PMAC','LESTER','CHIEF','KAPPAZ','GARTER','RICHO','MELONS','JMERC'))

chart_data <- coach %>%
  full_join(dates, by=character(0)) %>%
  left_join(wins, by='coach') %>%
  mutate(win_date = ifelse(win_date <= date, win_date, as.Date('2013-03-01'))) %>%
  group_by(coach, date) %>%
  summarise(
    last_win = as.Date(max(win_date, na.rm=T)),
    .groups='drop'
  ) %>%
  mutate(days = as.numeric(date - last_win)) %>%
  mutate(days_lbl = paste0(" ",round(days))) %>%
  arrange(date, desc(days), coach) %>%
  group_by(date) %>%
  mutate(rank = row_number()) %>%
  ungroup()

#chart_data <- chart_data %>% filter(date == '2020-01-01')

charts <- ggplot(chart_data, aes(rank, group=coach, fill=as.factor(coach), color=as.factor(coach))) +
  geom_tile(aes(y=days/2,
                height = days,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(coach)), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=days,label = days_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous( labels = scales::comma) +
  scale_x_reverse() +
  guides(color = 'none', fill = 'none') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
  

anim <- charts + transition_states(date, transition_length = 20, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GUESS THE STAT')


animate(anim, fps = 20, duration = 20, width = 800, height = 600, 
        renderer = gifski_renderer("ASL_bants.gif"))


chart_data %>%
  filter(date == '2022-05-01')

