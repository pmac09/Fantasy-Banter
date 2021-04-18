library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fireData)
library(tidyverse)
library(rjson)
library(data.table)

#setwd('./ASL-Dashboard')

## Source R Functions
listFiles <- list.files('./functions')
listFunctions <- unlist(lapply(listFiles[grepl('.R$', listFiles)], function(x) paste0('./functions/',x)))
sapply(listFunctions, source)
