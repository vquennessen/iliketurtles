initialize_population <- function(max_age, start_year, end_year, betas, 
                                  scenarios, hatch_success_stochasticity, 
                                  hatch_success_a, hatch_success_b, 
                                  hatch_success_mu, temp_mu, age_maturity,
                                  burn_in, F_survival, M_survival, 
                                  remigration_int, nests_mu, nests_sd, 
                                  eggs_mu, eggs_sd, temp_sd, 
                                  climate_stochasticity) {
  
  # age classes
  ages <- 1:max_age   # all age classes
  
  # years
  years <- seq(from = start_year, to = end_year)
  
  # dimensions
  no_betas <- length(betas)                     # number of mating functions
  no_scenarios <- length(scenarios)             # number of climate scenarios
  a <- length(ages)                             # number of age classes
  y <- length(years)                            # number of years to run
  
  # initialize hatching success
  # determine hatching success
  if (hatch_success_stochasticity == TRUE) {
    hatch_success <- array(rbeta(n = y * no_betas * no_scenarios, 
                                 shape1 = hatch_success_a, 
                                 shape2 = hatch_success_b), 
                           dim = y * no_betas * no_scenarios)
  } else {
    hatch_success <- array(rep(hatch_success_mu, times = y * no_betas * no_scenarios), 
                           dim = y * no_betas * no_scenarios)
  }
  
  # incubation temperature
  temperature <- temp_mu
  
  # initialize population size array by age class and sex
  N <- array(rep(NA, times = 2 * a * y * no_betas * no_scenarios), 
             dim = c(2, a, y, no_betas, no_scenarios))
  
  # add initial population size estimates to N
  
  # breeding females - 2 per age (112 females tagged S1-S3)
  N[1, age_maturity:(age_maturity + 56), , ,] <- 2
  
  # breeding males - 1 per age (33 males tagged in S1-S3)
  N[2, age_maturity:(age_maturity + 32), , ,] <- 1
  
  # move population forward in time burn_in years
  for (t in 1:burn_in) {
    
    for (b in 1:length(betas)) {
      
      # reproduction
      rep_output <- reproduction(N, age_maturity, max_age, years, t, betas, b, 
                                 scenarios = 'SSP2-4.5', s = 1, 
                                 remigration_int, nests_mu, nests_sd, eggs_mu, 
                                 eggs_sd, hatch_success, temp = temp_mu, 
                                 temp_sd, climate_stochasticity) 
      
      N <- rep_output[[1]]
      
      # population dynamics
      # survival for each age 
      for (a in 2:max_age) {
        
        # annual survival - females
        N[1, a, t + 1, , ] <- F_survival * N[1, a - 1, t, b, s]
        
        # annual survival - males
        N[2, a, t + 1, , ] <- M_survival * N[2, a - 1, t, b, s]
        
      }
      
    }
    
  }
  
}