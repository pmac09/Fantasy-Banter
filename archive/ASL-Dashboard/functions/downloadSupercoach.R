## Function to scrape supercoach data from stats centre
## Requires a valid access token

downloadSupercoach <- function(token){
  
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
  
  return(supercoach)
  
}
