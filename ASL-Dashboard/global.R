library(shiny)
library(reticulate)

#setwd('./ASL-Dashboard')
#devtools::install_github("rstudio/rsconnect", ref='737cd48')


## Source Python Functions
#virtualenv_create(envname = "pyEnv", python = "python3")
#virtualenv_install(envname = "pyEnv", packages = c('oauthlib', 'requests_oauthlib'))
#use_virtualenv("pyEnv", required = TRUE)

py_install(packages= c('oauthlib', 'requests_oauthlib'))
source_python('./functions/getToken.py')

## Source R Functions
listFiles <- list.files('./functions')
listFunctions <- unlist(lapply(listFiles[grepl('.R$', listFiles)], function(x) paste0('./functions/',x)))
sapply(listFunctions, source)

