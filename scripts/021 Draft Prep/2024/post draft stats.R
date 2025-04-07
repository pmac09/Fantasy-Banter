source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/supercoach_functions_V2.R') # import supercoach authentication variables
source('/Users/paulmcgrath/Github/Fantasy-Draft-Assistant/shiny/functions/firebase_functions.R') # import supercoach authentication variables


firebaseDownload <- function(projectURL, path = NULL){
  data <- suppressWarnings(download(projectURL, paste0('/Fantasy-Draft-Assistant/',path)))
  return(data)
} # Download data from firebase location

data <- firebaseDownload(projectURL, 'data/f1G5DPyzNg8o/')

draftTable <- bind_rows(data$draft_table) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  mutate(min = as_datetime(time) - lag(as_datetime(time)))

write.csv(draftTable, 'ASL_DRAFT_2025.csv', na='', row.names = T)



url <- "https://dfsaustralia.com/wp-admin/admin-ajax.php"
headers <- add_headers(
  "accept" = "*/*",
  "accept-language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "cache-control" = "no-cache",
  "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
  "pragma" = "no-cache",
  "priority" = "u=1, i",
  "sec-ch-ua" = "\"Google Chrome\";v=\"131\", \"Chromium\";v=\"131\", \"Not_A Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?1",
  "sec-ch-ua-platform" = "\"Android\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-origin",
  "x-requested-with" = "XMLHttpRequest"
)

posList <- c('DEF','MID','RUC','FWD')
dfs_data <- tibble()

for (pos in posList){
  body <- list(action = "afl_supercoach_big_board_call", position = pos)
  body_encoded <- URLencode(paste(names(body), body, sep="=", collapse="&"))
  
  # Send the POST request
  response <- POST(url, headers, body = body_encoded, encode = "form")
  
  # Extract and print the response content
  content_text <- content(response, "text")
  content_list <- fromJSON(content_text)
  
  # transform to dataframe
  dfs_posData <- content_list$last %>%
    mutate(across(
      where(~ all(suppressWarnings(is.na(.) | !is.na(as.numeric(.))))),
      \(x) suppressWarnings(as.numeric(x))
    )) %>%
    mutate(dfsFilter = pos)
  
  # bind results 
  dfs_data <- bind_rows(dfs_data, dfs_posData)
}

dfs_data <-dfs_data %>%
  mutate(feedID = as.numeric(substr(playerId,5,999))) %>%
  group_by(playerId) %>%
  arrange(rankAdj) %>%
  filter(row_number() == 1)


draftTable <- bind_rows(data$draft_table) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  left_join(dfs_data, by=c('player_id'='feedID')) %>%
  mutate(pickTime = as_datetime(time) - lag(as_datetime(time)))


## PICK TIME
draftTable %>%
  group_by(coach) %>%
  summarise(
    median = median(min, na.rm=T),
    average = round(mean(min, na.rm=T),0)
  ) %>%
  arrange(desc(median))


## Age
draftTable %>%
  group_by(coach) %>%
  summarise(
    median = median(age, na.rm=T),
    average = round(mean(age, na.rm=T),0)
  ) %>%
  arrange(desc(median))


## Adjusted Average
draftTable %>%
  group_by(coach) %>%
  summarise(
    median = median(FPadj, na.rm=T),
    average = round(mean(FPadj, na.rm=T),0)
  ) %>%
  arrange(desc(median))

