library(fitzRoy)
library(tidyverse)

data <- fetch_squiggle_data(query = "tips", year = year(Sys.Date()))

data1 <- data %>%
  filter(sourceid == 1) %>%
  filter(round >= 7)

data2 <- data1 %>%
  select(round, hteam, ateam, hconfidence) %>%
  mutate(home = 1) %>%
  mutate(hconfidence=as.numeric(hconfidence))

names(data2) <- c('round', 'team', 'oppo', 'conf', 'home')

data3 <- data2 %>%
  mutate(home = 0) %>%
  mutate(conf = 100-conf)

names(data3) <- c('round', 'oppo', 'team', 'conf', 'home')

data4 <- bind_rows(data2, data3) %>%
  arrange(round, desc(conf)) %>%
  group_by(round) %>%
  mutate(rank = row_number())


### BEST CHANCE

gauntlet <-  tibble(round=6, team='Dummy', oppo='Dummy', conf=0, rank=83393) %>%
  add_row(round=7, team='Melbourne', oppo='Richmond', conf=0, rank=75394)


data5 <- data4 %>%
  filter(!(round %in% gauntlet$round)) %>%
  filter(!(team %in% gauntlet$team)) %>%
  group_by(team) %>%
  arrange(desc(conf)) %>%
  mutate(bestChance = row_number()) %>%
  ungroup() %>%
  filter(bestChance <=3)


data6 <- split(data5$team, data5$round)

# Backtracking function
find_unique_combinations <- function(cmb, index = 1, current = character(), used = character()) {
  if (index > length(cmb)) {
    return(list(current))
  }
  
  results <- list()
  for (team in cmb[[index]]) {
    if (!(team %in% used)) {
      res <- find_unique_combinations(cmb, index + 1, c(current, team), c(used, team))
      results <- c(results, res)
    }
  }
  
  return(results)
}

# Run it
data7 <- find_unique_combinations(data6)

combo_df_list <- map(data7, ~ tibble(round = as.integer(names(data6)), team = .x))

# Join each combination with data5 to get the confidence
combo_with_conf <- map(combo_df_list, ~ left_join(.x, data5 %>% select(round, team, oppo, conf), by = c("round", "team")))

# Example: View the first combination with confidence
print(combo_with_conf[[1]])

# If you want them all together in one big dataframe:
all_combos_df <- bind_rows(combo_with_conf, .id = "combo_id")

x <- all_combos_df %>%
  group_by(combo_id) %>%
  summarise(
    total = mean(conf),
    min = min(conf)
  ) %>%
  arrange(desc(min), desc(total))
  
all_combos_df %>%
  filter(combo_id == x$combo_id[[1]])






