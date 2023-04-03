# initialize arrays

initialize_arrays <- function(start_year, end_year, A, Y, 
                              hatch_success_stochasticity, hatch_success_a, 
                              hatch_success_b, hatch_success_mu, scenario, 
                              F_survival_years, F_survival_values, 
                              M_survival_years, M_survival_values, 
                              temp_mu, logit_a, logit_b) {
  
  # years
  years <- seq(from = start_year, to = end_year)
  
  # initialize hatching success
  # determine hatching success
  if (hatch_success_stochasticity == TRUE) {
    hatch_success <- rbeta(n = Y, 
                           shape1 = hatch_success_a, 
                           shape2 = hatch_success_b)
  } else {
    hatch_success <- rep(hatch_success_mu, times = Y)
  }
  
  # initialize temperature scenarios
  temperatures <- rep(NA, Y)
  
  # other scenarios
  # TODO 
  
  if (scenario == 'SSP1-1.9') {
    
    temp1 <- temp_mu + 0.65
    temp2 <- temp1 + 0.1
    temp3 <- temp2 - 0.2
    
  } else if (scenario == 'SSP1-2.6') {
    
    temp1 <- temp_mu + 0.65
    temp2 <- temp1 + 0.2
    temp3 <- temp2 + 0.1
    
  } else if (scenario == 'SSP2-4.5') {
    
    temp1 <- temp_mu + 0.65
    temp2 <- temp1 + 0.5
    temp3 <- temp2 + 0.7
    
  } else if (scenario == 'SSP3-7.0') {
    
    temp1 <- temp_mu + 0.65
    temp2 <- temp1 + 0.6
    temp3 <- temp2 + 1.5
    
  } else if (scenario == 'SSP5-8.5') {
    
    temp1 <- temp_mu + 0.75
    temp2 <- temp1 + 0.8
    temp3 <- temp2 + 2
    
  }
  
  # years in IPCC climate report Table SPM.1  
  year1 <- 2040
  year2 <- 2060
  year3 <- 2100
  
  index1 <- which(years == year1)
  temperatures[1:index1] <- seq(from = temp_mu, to = temp1, 
                                length = (index1))
  
  index2 <- which(years == year2)
  temperatures[(index1 + 1):index2] <- seq(from = temp1, to = temp2,
                                           length = index2 - index1)
  
  index3 <- which(years == year3)
  temperatures[(index2 + 1):index3] <- seq(from = temp2, to = temp3,
                                           length = index3 - index2)
  
  # survival values vector - females
  F_survival <- rep(F_survival_values, times = F_survival_years)
  
  # survival values vector - males
  M_survival <- rep(M_survival_values, times = M_survival_years)
  
  # check it's long enough, and if not, add the last survival_value until it is
  # females
  if (length(F_survival) < A) {
    F_survival <- c(F_survival, rep(F_survival_values[length(F_survival_values)], 
                                    A - length(F_survival)))
  }
  
  # males
  if (length(M_survival) < A) {
    M_survival <- c(M_survival, rep(M_survival_values[length(M_survival_values)], 
                                    A - length(M_survival)))
  }
  
  # make female leslie matrix for survival
  f_matrix <- matrix(diag(F_survival[1:(A - 1)]), ncol = A - 1)
  f_Leslie <- rbind(rep(0, A), cbind(f_matrix, rep(0, A - 1)))
  
  # make male leslie matrix for survival
  m_matrix <- matrix(diag(M_survival[1:(A - 1)]), ncol = A - 1)
  m_Leslie <- rbind(rep(0, A), cbind(m_matrix, rep(0, A - 1)))
  #
  # # initialize population size array by age class and sex
  # N_output <- initialize_population(ages, no_betas, betas, no_scenarios, 
  #                                   scenarios, a, temp_mu, age_maturity, 
  #                                   burn_in = 150, max_age, remigration_int, 
  #                                   nests_mu, nests_sd, eggs_mu, eggs_sd,
  #                                   hatch_success = hatch_success_mu, 
  #                                   f_Leslie, m_Leslie)
  
  N <- initialize_population2(temp_mu, logit_a, logit_b, A, Y, 
                              F_survival, M_survival)
  
  # output
  output <- list(years, hatch_success, temperatures, N, 
                 F_survival, M_survival, f_Leslie, m_Leslie)
  
  return(output)
  
}