initialize_population <- function(ages, no_betas, betas, no_scenarios, 
                                  scenarios, a, temp_mu, age_maturity, 
                                  burn_in, max_age, remigration_int, 
                                  nests_mu, nests_sd, eggs_mu, eggs_sd,
                                  hatch_success, f_Leslie, m_Leslie) {
  
  # initialize population size array by age class and sex
  N <- array(rep(NA, times = 2 * a * burn_in * no_betas * no_scenarios), 
             dim = c(2, a, burn_in, no_betas, no_scenarios))
  
  # add initial population size estimates to N at t = 1
  
  # females - 2 per age (112 females tagged S1-S3)
  N[1, 2:max_age, 1, , ] <- 100
  
  # proportion male based on average temperature
  prop_male <- exp(logit_a + logit_b*temp_mu) / (1 + exp(logit_a + logit_b*temp_mu))
  
  # female hatchlings
  N[1, 1, 1, , ] <- round(111500 * (1 - prop_male))
  
  # male hatchlings
  N[2, 1, 1, , ] <- round(111500 * prop_male)
  
  # males - 1 per age (33 males tagged in S1-S3)
  N[2, 2:max_age, 1, , ] <- 100
  
  # move population forward in time burn_in years
  for (y in 2:burn_in) {
    
    for (b in 1:no_betas) {
      
      for (s in 1:no_scenarios) {
        
        # reproduction
        rep_output <- reproduction(N, age_maturity, max_age, years, y, betas, b, 
                                   scenarios = 'SSP2-4.5', s = 1, 
                                   remigration_int, nests_mu, nests_sd,
                                   eggs_mu, eggs_sd, 
                                   hatch_success = hatch_success_mu,
                                   temp = temp_mu, temp_sd, 
                                   climate_stochasticity = FALSE) 
        
        N <- rep_output[[1]]
        
        # population dynamics
          # annual survival - females
          N[1, , y, b, s] <- round(f_Leslie %*% N[1, , y - 1, b, s])
          
          # annual survival - males
          N[2, , y, b, s] <- round(m_Leslie %*% N[2, , y - 1, b, s])
        
      }
      
    }
    
    print(y)
    
  }
  
  # output of N population size array
  return(N)
  
}
