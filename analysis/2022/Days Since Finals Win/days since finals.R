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


