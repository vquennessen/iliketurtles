# initialize arrays

initialize_arrays <- function(max_age, years, no_scenarios, survival_years,
                              survival_values) {
  
  # age classes
  ages <- 1:max_age # all age classes
  a <- length(ages) # number of age classes
  
  # initialize population size array by age class and sex
  N <- array(rep(NA, times = 2 * a * years * no_scenarios), 
             dim = c(2, a, years, no_scenarios))
  
  # survival values vector
  survival <- rep(survival_values, times = survival_years)
  
  # check it's long enough, and if not, add the last survival_value until it is
  if (length(survival) < max_age) {
    survival <- c(survival, rep(survival_values[length(survival_values)], 
                                max_age - length(survival)))
  }
                
  
  output <- list(N, survival)
  
  return(output)
  
}