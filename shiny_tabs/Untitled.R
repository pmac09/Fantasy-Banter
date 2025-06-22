source('R/fantasy-banter-functions.R')



league %>%
  group_by(coach ,opponent_coach) %>%
  summarise(
    w=max(win, na.rm=T),
    l=sum(loss, na.rm=T)
  ) %>%
  mutate(pcnt=w/(w+l)) %>%
  arrange(desc(pcnt))
 



