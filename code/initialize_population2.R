initialize_population2 <- function(temp_mu, logit_a, logit_b, A, Y,
                                   F_survival, M_survival) {
  
  # difference in average global temperature of 0.7
  # source: https://www.climate.gov/news-features/understanding-climate/climate-change-global-temperature
  temp_1937 <- temp_mu - 0.7
  temp_2022 <- temp_mu
  
  # vector of temperatures
  temps <- seq(from = temp_1937, to = temp_2022, length = 2022 - 1937)
  
  # subsequent vector of male proportions based on temperatures
  props_male <- exp(logit_a + logit_b*temps) / (1 + exp(logit_a + logit_b*temps))
  
  # pull hatchlings from 15000 to 25000 for each year
  init_hatchlings <- sample(x = 10000:18500, size = A)
  
  # initial female and male hatchlings
  init_females <- floor(init_hatchlings*(1 - props_male))
  init_males <- floor(init_hatchlings*props_male)
  
  # initialize population size array by age class and sex
  init_N <- array(rep(0, times = 2 * A * Y), dim = c(2, A, Y))  
  
  # first age class
  init_N[1, 1, 1] <- init_females[1]
  init_N[2, 1, 1] <- init_males[1]  
  
  # for each year 
  for (i in 2:A) {
    
    # females
    init_N[1, i, 1] <- floor(init_females[i]*prod(F_survival[1:i]))
    
    # males
    init_N[2, i, 1] <- floor(init_males[i]*prod(M_survival[1:i]))
    
  }
  
  return(init_N)
  
}