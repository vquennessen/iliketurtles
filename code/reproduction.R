# reproduction

reproduction <- function(N, age_maturity, max_age, years, t, betas, b, 
                         scenarios, s, remigration_int, nests_mu, nests_sd,
                         eggs_mu, eggs_sd, hatch_success,
                         temp, temp_sd, climate_stochasticity) {
  
  # extract breeding adults from N
  
  # females only breed every remigration_int years
  n_breeding_F <- floor(sum(N[1, age_maturity:max_age, t, b, s], na.rm = TRUE) / remigration_int)
  
  # males mate every year???
  n_breeding_M <- floor(sum(N[2, age_maturity:max_age, t, b, s], na.rm = TRUE))
  
  # proportion of males
  prop_males <- n_breeding_M / (n_breeding_M + n_breeding_F)
  
  # relate prop_males to breeding success via mating function
  breeding_success <- pbeta(prop_males, shape1 = 1, shape2 = betas[b])
  
  # number of nests per female
  nests <- round(rnorm(n = n_breeding_F, mean = nests_mu, sd = nests_sd))
  
  # number of eggs per nest
  DF <- data.frame(Nests = nests, 
                   Eggs = rep(NA, times = length(nests)))
  
  for (n in 1:length(nests)) {
    
    DF$Eggs[n] <- sum(round(rnorm(n = DF$Nests[n], 
                                  mean = eggs_mu, 
                                  sd = eggs_sd)))
    
  }
  
  # total hatchlings = total eggs * hatching success * breeding_success
  hatchlings <- sum(DF$Eggs) * hatch_success[t, b, s] * breeding_success
  
  # for current temperature 
  if (climate_stochasticity == TRUE) {
    temperature <- rnorm(mean = temp, sd = temp_sd)
  } else {
    temperature <- temp
  }
  
  # determine proportion of male hatchlings based on temperature
  prop_male <- exp(logit_a + logit_b*temperature) / (1 + exp(logit_a + logit_b*temperature))
  
  # number of male and female hatchlings
  female_hatchlings <- round(hatchlings * (1 - prop_male))
  male_hatchlings <- round(hatchlings * prop_male)
  
  # add to population size array
  N[1, 1, t + 1, b, s] <- female_hatchlings
  N[2, 1, t + 1, b, s] <- male_hatchlings
  
  output <- list(N)
  
  return(output)
  
}
