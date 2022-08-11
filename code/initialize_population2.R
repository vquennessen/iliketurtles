initialize_population2 <- function(hatchlings, F_survival, M_survival, max_age, prop_male) {
  
  # difference in average global temperature of 0.7
  temp_1937 <- temp_mu - 0.7
  temp_2022 <- temp_mu
  
  temps <- seq(from = temp_1937, to = temp_2022, length = 2022 - 1937 + 1)
  
  props_male <- exp(logit_a + logit_b*temps) / (1 + exp(logit_a + logit_b*temps))

  
  # pull hatchlings from 15000 to 25000 for each year
  init_hatchlings <- sample(x = 18580:25000, size = max_age)
  
  # initial female and male hatchlings
  init_females <- round(init_hatchlings*(1 - prop_male))
  init_males <- round(init_hatchlings*prop_male)
  
  # initialize population size array by age class and sex
  init_N <- array(rep(NA, times = 2 * a), dim = c(2, a))  
  
  # first age class
  init_N[1, 1] <- init_females[1]
  init_N[2, 1] <- init_males[1]
  
  # for each year 
  for (i in 2:max_age) {
    
    # females
    init_N[1, i] <- round(init_females[i]*prod(F_survival[1:i]))
    
    # males
    init_N[2, i] <- round(init_males[i]*prod(M_survival[1:i]))
    
  }
}