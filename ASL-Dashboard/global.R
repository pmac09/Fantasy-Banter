library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fireData)
library(tidyverse)
library(reticulate)
library(rjson)

#setwd('./ASL-Dashboard')

## Source R Functions
listFiles <- list.files('./functions')
listFunctions <- unlist(lapply(listFiles[grepl('.R$', listFiles)], function(x) paste0('./functions/',x)))
sapply(listFunctions, source)

## Initiate Python
py_install(packages= c('oauthlib', 'requests_oauthlib'))
import('oauthlib')
#source_python('./functions/getToken.py')
