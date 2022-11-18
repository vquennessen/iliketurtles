# reproduction

reproduction <- function(n_breeding_M, n_breeding_F, beta,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success, climate_stochasticity, 
                         temp, temp_sd, logit_a, logit_b, N, y) {

  # extract breeding adults from N
  
  # proportion of males
  prop_males <- 2*(n_breeding_M / (n_breeding_M + n_breeding_F))
  
  # relate prop_males to breeding success via mating function
  breeding_success <- pbeta(prop_malcalculatees, shape1 = 1, shape2 = beta)
  
  # number of nests per female
  nests <- floor(rnorm(n = n_breeding_F, mean = nests_mu, sd = nests_sd))
  
  # replace any zeros or -1 with +1
  nests[which(nests < 1)] <- 1
  
  # initialize eggs vector
  eggs <- rep(NA, times = n_breeding_F)

  # number of eggs per nest
  for (f in 1:n_breeding_F) {
    
    eggs[f] <- sum(floor(rnorm(n = nests[f], mean = eggs_mu, sd = eggs_sd)))
    
  }
  
  # total hatchlings = total eggs * hatching success * breeding_success
  hatchlings <- sum(eggs) * hatch_success * breeding_success
  
  # for current temperature 
  if (climate_stochasticity == TRUE) {
    temperature <- rnorm(mean = temp, sd = temp_sd)
  } else {
    temperature <- temp
  }
  
  # determine proportion of male hatchlings based on temperature
  prop_male <- exp(logit_a + logit_b*temperature) / (1 + exp(logit_a + logit_b*temperature))
  
  # number of male and female hatchlings
  female_hatchlings <- floor(hatchlings * (1 - prop_male))
  male_hatchlings <- floor(hatchlings * prop_male)

  # output
  output <- list(female_hatchlings, male_hatchlings)
  
  return(output)
  
}
