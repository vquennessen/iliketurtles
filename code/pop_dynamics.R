# population dynamics

pop_dynamics <- function(F_survival, M_survival, t) {
  
  # annual survival - females
  N[1, , t + 1, s] <- F_survival * N[1, , t, s]
  
  # annual survival - males
  N[2, , t + 1, s] <- M_survival * N[2, , t, s]
  
  # output
  output <- list(N)
  
  return(output)
  
}