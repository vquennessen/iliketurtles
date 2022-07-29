# base model

# set working directory
setwd("~/Projects/iliketurtles/code")

##### parameters ###############################################################

# turtle demographic
max_age <- 85                                         # lifespan
F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.929)  # survival per stage - F
M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.490)  # survival per stage - M
mf_beta <- c(1, 10, 100)                              # mating function beta
no_betas <- length(mf_beta)                           # num. of mating functions
pivotal_temp <- 29.3

# model parameters
years <- 200                                      # years to run model  
scenarios <- c('Same')                            # climate scenarios
no_scenarios <- length(scenarios)                 # number of climate scenarios


##### source initialized arrays ################################################

init_output <- initialize_arrays(max_age, years, no_betas, no_scenarios, 
                                 F_survival_years, F_survival_values, 
                                 M_survival_years, M_survival_values)

N <- init_output[[1]]            # population size array
F_survival <- init_output[[2]]   # vector of survival values - females
M_survival <- init_output[[3]]   # vector of survival values - males

##### model ####################################################################

for (t in 1:years) {
  
  # population dynamics
  pop_output <- pop_dynamics(F_survival, M_survival, t) 
  
  N <- init_output[[1]]            # population size array
  
  # climate change temperature estimates
  temp_output <- temperature()
  
  temp_mu <- output[[1]]           # temperature mean
  temp_var <- output[[2]]          # temperature variance
  
  # reproduction
  
  
}




