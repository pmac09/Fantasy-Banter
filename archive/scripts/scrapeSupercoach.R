library(reticulate)
library(rjson)
library(fireData)


## Initiate python environment
py_install(packages= c('oauthlib', 'requests_oauthlib'))
source_python('./functions/getToken.py')

## Get environment variables
cid <- Sys.getenv('SC_CID')
usr <- Sys.getenv('SC_USR')
pwd <- Sys.getenv('SC_PWD')

## Get Supercoach access token using python function
token <- getToken(cid, usr, pwd)

## Scrape Supercoach stats centre
stats.url <- paste0('https://supercoach.heraldsun.com.au/afl/draft/statscentre?access_token=', token$access_token)
stats.html <- strsplit(readLines(stats.url), "\n")
stats.string <- stats.html[[grep("var researchGridData",stats.html)]]
stats.data <- fromJSON(substr(stats.string,24,nchar(stats.string)))


## Prepare data for loading to firebase
supercoach <- list()
for(i in 1:length(stats.data)){
  id <- paste0(stats.data[[i]]$id)
  supercoach[[id]] <- stats.data[[i]]
}

## Get environment variables
DATABASE_URL <- Sys.getenv('DATABASE_URL')

## Upload to firebase
round <- paste0('2019-23 ')
write(toJSON(supercoach), paste0('./data/SC-Player-Scores-',round,'.json'))
patch(supercoach, DATABASE_URL, directory = paste0("ASL_DASHBOARD/SUPERCOACH/",round))
patch(supercoach, DATABASE_URL, directory = "ASL_DASHBOARD/SUPERCOACH/LIVE")



