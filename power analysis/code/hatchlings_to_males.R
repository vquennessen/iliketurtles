#' how many hatchlings should be sampled from a nest to robustly estimate the 
#' number of males that contributed

# load libraries
library(ggplot2)

# define n() function to get length(unique)
n <- function(x) {length(unique(x))}

# population parameters
n_eggs         <- 100    # number of eggs per nest
max_hatchlings <- 100   # maximum number of hatchlings that can be sampled
max_males      <- 10     # maximum number of males females can mate with
max_eggs       <- min(max_hatchlings, n_eggs) 

# model parameters
n_sims         <- 100000
cutoff         <- c(0.9, 0.95, 0.99, 1)

# pre-allocate data frame
DF <- data.frame(Males = rep(1:max_males, each = (max_eggs - 1)), 
                 Sample_size = rep(2:max_eggs, times = max_males), 
                 Proportion_correct = rep(NA, dim = max_males*(max_eggs - 1)))

# for each number of males that contribute to a nest:
for (i in 1:max_males) {
  
  # proportion_correct array
  prop_correct <- rep(NA, n_sims)
  
  # for each sample size
  for (j in 2:max_eggs) {
    
    # pre-allocate correct identifications of number of males
    correct <- rep(NA, n_sims)
    
    for (k in 1:n_sims) {
      
      # simulate male contributions to nest
      nest <- sample(1:i, size = max_eggs, replace = TRUE)
      
      # take samples of size k
      samples <- sample(nest, size = j, replace = FALSE)
      
      # correct allocation of number of males?
      correct[k] <- length(unique(samples)) == i
      
    }
    
    # calculate index in data frame
    index <- (i - 1)*(max_eggs - 1) + j - 1
    
    # stick proportion in data frame
    DF$Proportion_correct[index] <- mean(correct)
  }
  
}

# plot results
ggplot(DF, aes(x = Sample_size, y = Proportion_correct, 
               col = as.factor(Males))) +
  geom_path(lwd = 1) +
  labs(col = 'Number \n of Males')

# print table of minimum sample sizes for robust estimate
