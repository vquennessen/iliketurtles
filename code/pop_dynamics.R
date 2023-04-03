# population dynamics

pop_dynamics <- function(max_age, t, b, s) {
  
  # for each age 
  for (a in 2:max_age) {
    
    # annual survival - females
    N[1, a, t + 1, , ] <- F_survival * N[1, a - 1, t, b, s]
    
    # annual survival - males
    N[2, a, t + 1, , ] <- M_survival * N[2, a - 1, t, b, s]
    
  }
  
  # output
  output <- list(N)
  
  return(output)
  
}