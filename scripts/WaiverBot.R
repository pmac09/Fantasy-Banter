library(beepr)

# Load supercoach functions
func_path <- '/Users/paulmcgrath/Github/Fantasy-Banter/functions/'
source(paste0(func_path, 'supercoach_functions_V2.R')) # import authentication variables

print_log("WAIVER BOT")

# Connect to supercoach
sc <- sc_setup(cid, tkn)

# determine next waiver time
url <- sc$url$playerStatus
data <- sc_download(sc$auth, url)

wvr <-  suppressMessages(format(as_datetime(min(unlist(lapply(data, function(x){x$waiver_until}))), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S"))
#wvr <- Sys.time()
wait <- round(as.numeric(difftime(wvr, Sys.time(), units=c('secs'))),0)-30
#wait <- -1

print_log(paste0('Waiting until waiver runtime: ', wvr))
if(wait >0) Sys.sleep(wait)

print_log(paste0('Start monitoring Free Agency... '))
fa_open <- FALSE


# loop until fa open
while(!fa_open){
  
  # wait to avoid DDOS
  Sys.sleep(1)
  
  # download player status data
  data <- tryCatch({sc_download(sc$auth, url)},
    error = function(cond) {
      print_log(paste0('ERROR'))
      NULL
    })
  
  if(is.null(data)) next
  
  # determine if waiver is open
  stat <- tibble(status = unlist(lapply(data, function(x){x$trade_status}))) %>%
    group_by(status) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  
  # update loop
  if(stat$status[[1]] == 'free_agent') fa_open <- TRUE
  #if(runif(1) > 0.8) fa_open <- TRUE
  
  # # print message
  print_log(ifelse(fa_open, 'Free Agency Open', 'Free Agency Closed'))
  
}  

beep()

 
 swxz# url <- 'https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/leagues/1283/userteam/713/claimWaiverPlayer/786/droppedWaiverPlayer/785'
# 
# x <- POST(
#   url = url,
#   config = sc$auth
# )
# 
# 
# # 
# # 
# url <- 'https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/userteams/713/players/'
# 
# x <- get_sc_data(sc$auth, url)
# # 
# # library(rjson)
# # 
# # payload <- '{"live_draft_mode":false,"selections":[{"player_id":639,"picked":"true","position":"DEF"},{"player_id":44,"picked":"true","position":"DEF"},{"player_id":194,"picked":"true","position":"DEF"},{"player_id":533,"picked":"true","position":"DEF"},{"player_id":164,"picked":"true","position":"DEF"},{"player_id":263,"picked":"false","position":"DEF"},{"player_id":526,"picked":"true","position":"MID"},{"player_id":242,"picked":"true","position":"MID"},{"player_id":372,"picked":"true","position":"MID"},{"player_id":286,"picked":"true","position":"MID"},{"player_id":193,"picked":"true","position":"MID"},{"player_id":312,"picked":"true","position":"MID"},{"player_id":686,"picked":"true","position":"MID"},{"player_id":87,"picked":"false","position":"MID"},{"player_id":443,"picked":"true","position":"RUC"},{"player_id":180,"picked":"true","position":"RUC"},{"player_id":566,"picked":"true","position":"FWD"},{"player_id":363,"picked":"true","position":"FWD"},{"player_id":31,"picked":"true","position":"FWD"},{"player_id":681,"picked":"true","position":"FWD"},{"player_id":151,"picked":"true","position":"FWD"},{"player_id":75,"picked":"false","position":"FWD"},{"player_id":788,"picked":"false","position":"DEF"}],"current":[{"player_id":639,"position":"DEF","picked":"true"},{"player_id":44,"position":"DEF","picked":"true"},{"player_id":194,"position":"DEF","picked":"true"},{"player_id":533,"position":"DEF","picked":"true"},{"player_id":164,"position":"DEF","picked":"true"},{"player_id":263,"position":"DEF","picked":"false"},{"player_id":526,"position":"MID","picked":"true"},{"player_id":242,"position":"MID","picked":"true"},{"player_id":372,"position":"MID","picked":"true"},{"player_id":286,"position":"MID","picked":"true"},{"player_id":193,"position":"MID","picked":"true"},{"player_id":312,"position":"MID","picked":"true"},{"player_id":686,"position":"MID","picked":"true"},{"player_id":87,"position":"MID","picked":"false"},{"player_id":443,"position":"RUC","picked":"true"},{"player_id":180,"position":"RUC","picked":"true"},{"player_id":566,"position":"FWD","picked":"true"},{"player_id":363,"position":"FWD","picked":"true"},{"player_id":31,"position":"FWD","picked":"true"},{"player_id":681,"position":"FWD","picked":"true"},{"player_id":151,"position":"FWD","picked":"true"},{"player_id":75,"position":"FWD","picked":"false"},{"player_id":785,"position":"DEF","picked":"false"}]}'
# # 
# # payload <- '[{"player_id":639,"position":"DEF","picked":"true"},{"player_id":44,"position":"DEF","picked":"true"},{"player_id":194,"position":"DEF","picked":"true"},{"player_id":533,"position":"DEF","picked":"true"},{"player_id":164,"position":"DEF","picked":"true"},{"player_id":263,"position":"DEF","picked":"false"},{"player_id":526,"position":"MID","picked":"true"},{"player_id":242,"position":"MID","picked":"true"},{"player_id":372,"position":"MID","picked":"true"},{"player_id":286,"position":"MID","picked":"true"},{"player_id":193,"position":"MID","picked":"true"},{"player_id":312,"position":"MID","picked":"true"},{"player_id":686,"position":"MID","picked":"true"},{"player_id":87,"position":"MID","picked":"false"},{"player_id":443,"position":"RUC","picked":"true"},{"player_id":180,"position":"RUC","picked":"true"},{"player_id":566,"position":"FWD","picked":"true"},{"player_id":363,"position":"FWD","picked":"true"},{"player_id":31,"position":"FWD","picked":"true"},{"player_id":681,"position":"FWD","picked":"true"},{"player_id":151,"position":"FWD","picked":"true"},{"player_id":75,"position":"FWD","picked":"false"},{"player_id":785,"position":"DEF","picked":"false"}]'
# # 
# url <- 'https://supercoach.heraldsun.com.au/2024/api/afl/draft/v1/userteams/713/players?formation_id=0'
# # 
# # x <- PUT(
# #   url = url,
# #   config = sc$auth,
# #   body = payload,
# #   encode = 'json'
# # )
# # 
# # parsed <- fromJSON(payload)
# # 
# # new_payload <- toJSON(payload)

