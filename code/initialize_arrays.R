# initialize arrays

initialize_arrays <- function(max_age, start_year, end_year, betas, 
                              scenarios, num_sims, temp_mu, temp_sd,
                              F_survival_years, F_survival_values, 
                              M_survival_years, M_survival_values) {
  
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
  
  # initialize temperature scenarios
  temperatures <- array(rep(NA, y * no_scenarios), dim = c(y, no_scenarios))
  
  for (ns in 1:no_scenarios) {
    
    # other scenarios
    # TODO 
    
    if (scenarios[ns] == 'SSP2-4.5') {
       
      year1 <- 2040
      temp1 <- temp_mu + 0.65
      
      year2 <- 2060
      temp2 <- temp1 + 0.5
      
      year3 <- 2100
      temp3 <- temp2 + 0.7
      
      index1 <- which(years == year1)
      temperatures[1:index1, ns] <- seq(from = temp_mu, to = temp1, 
                                        length = (index1))
      
      index2 <- which(years == year2)
      temperatures[(index1 + 1):index2, ns] <- seq(from = temp1, to = temp2,
                                                   length = index2 - index1)
      
      index3 <- which(years == year3)
      temperatures[(index2 + 1):index3, ns] <- seq(from = temp2, to = temp3,
                                                   length = index3 - index2)
      
    }
    
  }
  
  # initialize population size array by age class and sex
  N <- initialize_population(max_age, start_year, end_year, betas, scenarios, 
                             hatch_success_stochasticity, hatch_success_a, 
                             hatch_success_b, hatch_success_mu, temp_mu, 
                             age_maturity, burn_in = 50, F_survival, M_survival, 
                             remigration_int, nests_mu, nests_sd, eggs_mu, 
                             eggs_sd, temp_sd, climate_stochasticity)
  
  # survival values vector - females
  F_survival <- rep(F_survival_values, times = F_survival_years)
  
  # survival values vector - males
  M_survival <- rep(M_survival_values, times = M_survival_years)
  
  # check it's long enough, and if not, add the last survival_value until it is
  # females
  if (length(F_survival) < max_age) {
    survival <- c(F_survival, rep(F_survival_values[length(F_survival_values)], 
                                  max_age - length(F_survival)))
  }
  
  # males
  if (length(M_survival) < max_age) {
    survival <- c(M_survival, rep(M_survival_values[length(M_survival_values)], 
                                  max_age - length(M_survival)))
  }
  
  # output
  output <- list(ages, years, hatch_success, temperatures, N, 
                 F_survival, M_survival)
  
  return(output)
  
}