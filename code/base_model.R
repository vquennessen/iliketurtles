# base model

# set working directory
setwd("~/Projects/iliketurtles/code")

# turtle demographic parameters
max_age <- 85                                         # lifespan
F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.929)  # survival per stage - F
M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.490)  # survival per stage - M

# model parameters
years <- 200                                      # years to run model  
scenarios <- c('Good', 'Same', 'Bad')             # climate scenarios
no_scenarios <- length(scenarios)                 # number of climate scenarios

# source initialized arrays
init_output <- initialize_arrays(max_age, years, no_scenarios, 
                                 F_survival_years, F_survival_values, 
                                 M_survival_years, M_survival_values)

N <- init_output[[1]]            # population size array
F_survival <- init_output[[2]]   # vector of survival values - females
M_survival <- init_output[[3]]   # vector of survival values - males

# population dynamics



