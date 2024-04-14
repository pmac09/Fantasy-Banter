library(googleCloudStorageR)


gcs_path <- paste0('fantasy-banter/master/league-data')
data_upload <- gcs_upload(file=fixture, name=gcs_path)


gcs_setup()


1## Fetch token. See: https://developers.google.com/identity/protocols/oauth2/scopes
scope <-c("https://www.googleapis.com/auth/cloud-platform")
token <- token_fetch(scopes = scope)

## Pass your token to gcs_auth
gcs_auth(token = token)

## Perform gcs operations as normal
gcs_list_objects()

path <- './data/2023/raw/'
files <- list.files(path)
files <- files[grep('ladderAndFixture',files)]

for (i in 70:length(files)){
  
  filepath <- paste0(path, 'completeStatspack?player_id=',i,'.rds')
  player_data <- readRDS(filepath)
  
  if(is.null(player_data)) next

  gcs_path <- paste0('fantasy-banter/master/players/history/playerStats-',
                     formatC(i, width = 3, format = "d", flag = "0"))
  
  upload_try <- gcs_upload(file=player_data,
                           name=gcs_path)
  
}

sort(as.numeric(gsub('.rds','',substr(files, 29, 100))))


upload_try <- gcs_upload(player_data, bucket = "fantasy-banter-082017.appspot.com")
objects <- gcs_list_objects( bucket = "fantasy-banter-082017.appspot.com")
parsed_download <- gcs_get_object(objects$name[[2]], bucket = "fantasy-banter-082017.appspot.com")



