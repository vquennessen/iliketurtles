# base model

base_model <- function(max_age, F_survival_years, F_survival_values, 
                       M_survival_years, M_survival_values, age_maturity, 
                       beta, remigration_int, nests_mu, nests_sd, 
                       eggs_mu, eggs_sd, hatch_success_mu, hatch_success_a, 
                       hatch_success_b, hatch_success_stochasticity, 
                       logit_a, logit_b, temp_mu, temp_sd, 
                       climate_stochasticity, start_year, end_year, scenario, 
                       A, Y) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(start_year, end_year, A, Y, 
                                   hatch_success_stochasticity, hatch_success_a, 
                                   hatch_success_b, hatch_success_mu, scenario, 
                                   F_survival_years, F_survival_values, 
                                   M_survival_years, M_survival_values, 
                                   temp_mu, logit_a, logit_b)
  
  years <- init_output[[1]]          # years to run model
  hatch_success <- init_output[[2]]  # array of hatching success values
  temperatures <- init_output[[3]]   # temperatures across climate scenarios
  N <- init_output[[4]]              # population size array
  F_survival <- init_output[[5]]     # vector of survival values - females
  M_survival <- init_output[[6]]     # vector of survival values - males
  f_Leslie <- init_output[[7]]       # female Leslie matrix
  m_Leslie <- init_output[[8]]       # male Leslie matrix
  
  ##### model ##################################################################
  for (y in 2:Y) {
    
    # population dynamics
    # survival for each age 
    
    # annual survival - females
    N[1, , y] <- floor(f_Leslie %*% N[1, , y - 1])
    
    # annual survival - males
    N[2, , y] <- floor(m_Leslie %*% N[2, , y - 1])
    
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y]) < 1 || sum(N[2, , y]) < 1) { break }
    
    # climate change temperature estimates
    temp <- temperatures[y]
    
    # reproduction
    rep_output <- reproduction(N, age_maturity, max_age, remigration_int, 
                               beta, nests_mu, nests_sd, eggs_mu, eggs_sd, 
                               hatch_success = hatch_success[y], 
                               climate_stochasticity, temp, temp_sd, 
                               logit_a, logit_b, y)
    
    # add recruits to population size array
    N[1, 1, y] <- rep_output[[1]]
    N[2, 1, y] <- rep_output[[2]]

  }
    
  #}
  
  ##### output #################################################################
  
  # create abundance array
  abundance <- colSums(N, dims = 2)
  mature_abundance <- colSums(N[, age_maturity:max_age, ], dims = 2)
  
  # output N and abundance arrays
  output <- list(N, abundance, mature_abundance)
  
  return(output)
  
}




