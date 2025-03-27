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
