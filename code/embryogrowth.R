#' modified from 
#' ../Monsinjon/Cm_Atlantic_Southwest_PauloLara/embryogrowth.R
#' save nesttemps object as .Rdata file
#' list of each nest, where each list item is a dataframe with:
#' avg daily temperature, datetime, datetime_UTC, 
#' and time (in seconds) since hatching

# set working directory
setwd("~/Projects/iliketurtles")

# load libraries
library(embryogrowth)
library(dplyr)

##### load nest temperatures ###################################################

# 1. load data
load("data/nests.Rdata")

complete_nests <- nests %>%
  select(Season, N_NINHO, DATA_OCORR, DATA_ECLOS, TEMP_LOGGER_ID) %>%
  na.omit()

complete_nests$nest_ID <- seq.int(nrow(complete_nests))

# load temperature data
load("data/temperature_data.Rda")

# add season_nest column
temps <- temps %>%
  group_by(Season, Nest) %>%
  mutate(Season_Nest = paste(as.character(Season), '_', as.character(Nest), sep = ''))



