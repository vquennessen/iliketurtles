# base model

base_model <- function(max_age, F_survival_years, F_survival_values, 
                       M_survival_years, M_survival_values, 
                       betas, remigration_int, nests_mu, nests_sd,
                       eggs_mu, eggs_sd, hatch_success, 
                       pivotal_temp, TRT, logit_a, logit_b, 
                       temp_mu, temp_sd, climate_stochasticity, 
                       start_year, end_year, scenarios, num_sims) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(max_age, start_year, end_year, betas, 
                                   scenarios, num_sims, temp_mu, temp_sd, 
                                   F_survival_years, F_survival_values, 
                                   M_survival_years, M_survival_values)
  
  ages <- init_output[[1]]           # ages
  years <- init_output[[2]]          # years to run model
  hatch_success <- init_output[[3]]  # array of hatching success values
  temperatures <- init_output[[4]]   # temperatures across climate scenarios
  N <- init_output[[5]]              # population size array
  F_survival <- init_output[[6]]     # vector of survival values - females
  M_survival <- init_output[[7]]     # vector of survival values - males
  
  ##### model ##################################################################
  
  for (t in 1:y) {
    
    for (b in 1:length(betas)) {
      
      for (s in 1:num_scenarios) {
        
        # population dynamics
        # survival for each age 
        for (a in 2:max_age) {
          
          # annual survival - females
          N[1, a, t + 1, , ] <- F_survival * N[1, a - 1, t, b, s]
          
          # annual survival - males
          N[2, a, t + 1, , ] <- M_survival * N[2, a - 1, t, b, s]
          
        }
        
        # climate change temperature estimates
        temp <- temperatures[t]
        
        # reproduction
        rep_output <- reproduction(N, age_maturity, max_age, years, t, betas, b, 
                                   scenarios, s, remigration_int, nests_mu, 
                                   nests_sd, eggs_mu, eggs_sd, 
                                   hatch_success = hatch_success[t, b, s],
                                   temp, temp_sd, climate_stochasticity) 
        
        N <- rep_output[[1]]
        
      }
      
    }
    
  }
  
  ##### output #################################################################
  
  # create abundance array
  abundance <- colSums(N, dims = 2)
  
  # output N and abundance arrays
  output <- list(N, abundance)
  
  return(output)
  
}




