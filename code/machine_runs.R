# machine runs

# set working directory
setwd('~/Projects/iliketurtles/code')

# source needed functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')

# model parameters
num_sims <- c(10000)
betas <- c(1, 2, 3, 5, 10, 20, 50, 100)
# betas <- c(20, 50, 100)
scenarios <- c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0', 'SSP5-8.5')

for (ns in 1:length(num_sims)) {
  
  for (b in 1:length(betas)) {
    
    for (s in 1:length(scenarios)) {
    
    run_base_model(num_sims = num_sims[ns], 
                   scenario = scenarios[s], 
                   beta = betas[b])
    
    }
    
  }
  
}

