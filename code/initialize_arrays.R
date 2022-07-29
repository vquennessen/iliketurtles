# initialize arrays

initialize_arrays <- function(max_age, years, no_betas, no_scenarios, 
                              F_survival_years, F_survival_values, 
                              M_survival_years, M_survival_values) {
  
  # age classes
  ages <- 1:max_age # all age classes
  a <- length(ages) # number of age classes
  
  # initialize population size array by age class and sex
  N <- array(rep(NA, times = 2 * a * years * no_betas, no_scenarios), 
             dim = c(2, a, years, no_betas, no_scenarios))
  
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
                
  
  output <- list(N, F_survival, M_survival)
  
  return(output)
  
}