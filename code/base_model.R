# base model

# set working directory
setwd("~/Projects/iliketurtles/code")

# turtle demographic parameters
max_age <- 85

# model parameters
years <- 200
scenarios <- c('Good', 'Same', 'Bad')
no_scenarios <- length(scenarios)

# source initialized arrays
init_output <- initialize_arrays(max_age, years, no_scenarios)

N <- init_output[[1]]          # population size array
survival <- init_output[[2]]   # vector of survival values

# population dynamics



