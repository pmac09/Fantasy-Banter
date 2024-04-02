# Load supercoach functions
func_path <- '/Users/paulmcgrath/Github/Fantasy-Banter/functions/'
source(paste0(func_path, 'supercoach_functions_V2.R')) # import authentication variables

sc <- sc_setup(cid, tkn)


get_player_data <- function(sc){
  
  player_list <- sc_players(sc)
  player_status <- sc_download(sc$auth, sc$url$playerStatus)
  
  player_data <- tibble(
    id = unlist(lapply(player_list, function(x) x$id)),
    feed_id = unlist(lapply(player_list, function(x) x$feed_id)),
    player_name = NA,
    first_name = unlist(lapply(player_list, function(x) x$first_name)),
    last_name = unlist(lapply(player_list, function(x) x$last_name)),
    team = unlist(lapply(player_list, function(x) x$team$abbrev)),
    pos = unlist(lapply(player_list, function(x) paste0(lapply(x$positions, function(p) p$position), collapse = '/'))),
    avg = unlist(lapply(player_list, function(x) x$previous_average)), # unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg)),
    avg3 = unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg3)),
    avg5 = unlist(lapply(player_list, function(x) x$player_stats[[1]]$avg5)),
    user_team_id = unlist(lapply(player_list, function(x) {
      y <- player_status[[as.character(x$id)]]$user_team_id
      if(is.null(y)) y <- NA
      return(y)
    }))
  ) %>%
    mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
    group_by(player_name, team) %>%
    mutate(n = n()) %>%
    mutate(player_name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), player_name)) %>%
    ungroup() %>%
    select(-n, -first_name)
  
  return(player_data)
}




league_data <- sc_league(sc)
team_names <- get_team_names(league_data)

team_id <- team_names[[1]]

team_data <- player_data %>%
  filter(user_team_id == team_id)

get_player_names <- function(team_data){
  print_log('get_player_names')
  player_names <- team_data$id
  names(player_names) <- team_data$player_name
  player_names <- player_names[order(team_data$last_name)]
  return(player_names)
}
player_names <- get_player_names(team_data)

player_id <- player_names[[1]]



# Generate all possible combinations
all_combinations <- expand.grid(x)

# Print all combinations
print(all_combinations)




