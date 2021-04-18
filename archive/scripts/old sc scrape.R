library(reticulate)
library(rjson)
library(fireData)

library(httr)
library(XML)


## Initiate python environment
py_install(packages= c('oauthlib', 'requests_oauthlib'))
source_python('./functions/getToken.py')

## Get environment variables
cid <- Sys.getenv('SC_CID')
usr <- Sys.getenv('SC_USR')
pwd <- Sys.getenv('SC_PWD')

## Get Supercoach access token using python function
token <- getToken(cid, usr, pwd)


url <- 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/settings'

pageData <- GET(url, add_headers(Authorization=paste0("Bearer ", token$access_token)))
XMLdata <- content(pageData, as="parsed", encoding="utf-8")
#doc <- xmlTreeParse(XMLdata, useInternal=TRUE)
#vList <- xmlToList(xmlRoot(doc))


url <- 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/me'

url <- 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/users/300791/stats'

url <- 'https://supercoach.heraldsun.com.au/afl/draft/other_teams?tid=397'

stats.html <- strsplit(readLines(url), "\n")



cid <- Sys.getenv('SC_CID')
usr <- Sys.getenv('SC_USR')
pwd <- Sys.getenv('SC_PWD')


temp <- GET(
  'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token',
  authenticate(usr, pwd)
)


supercoach <- oauth_endpoint(
  authorize = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token',
  access = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token'
)
url = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token'



google_app <- oauth_app(
  "google",
  key = "123456789.apps.googleusercontent.com",
  secret = "abcdefghijklmnopqrstuvwxyz"
)  


yahoo <- httr::oauth_endpoint(  authorize = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token',
                                access = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token')

myapp <- httr::oauth_app("yahoo", key=cKey, secret = cSecret, redirect_uri = "oob")





httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r", redirect_uri = myapp$redirect_uri))
passcode = readline(prompt="Enter Passcode: ") 
yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code=passcode)










