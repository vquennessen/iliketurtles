# population dynamics

pop_dynamics <- function(F_survival, M_survival, t) {
  
  # for each mating function beta value
  for (b in 1:no_betas) {
    
    # for each climate change scenario
    for (s in 1:no_scenarios) {
      
      # annual survival - females
      N[1, , t + 1, , ] <- F_survival * N[1, , t, b, s]
      
      # annual survival - males
      N[2, , t + 1, , ] <- M_survival * N[2, , t, b, s]
      
    }
    
  }
  
  # output
  output <- list(N)
  
  return(output)
  
}