### function for number of eggs to sample to determine the number of sires

eggs_to_sample <- function(n_eggs = 100,         # number of eggs per nest
                           max_hatchlings = 96,  # max hatchlings sampled
                           max_males = 7,        # max # of M F can mate with
                           breeding,             # breeding mode
                           n_sims = 100000)      # number of simulations to run
  {
  
  max_eggs <- min(max_hatchlings, n_eggs) 
  
  
  # pre-allocate data frame
  DF <- data.frame(Males = rep(1:max_males, each = (max_eggs - 1)), 
                   Sample_size = rep(2:max_eggs, times = max_males), 
                   Proportion_correct = rep(NA, dim = max_males*(max_eggs - 1)))
  
  
  # for each number of males that contribute to a nest:
  for (i in 1:max_males) {
    
    if (i > 1) {
      # set contributions per males
      if (breeding == 'dominant') {
        MC <- 0.8
        contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      } else if (breeding == 'exponential') {
        MC <- 0.5
        contributions <- 0.5^c(1:(i-1))
        contributions <- c(contributions, contributions[i-1])
      } else if (breeding == 'random') {
        contributions <- rep(1/i, i)
      }
    } else { contributions <- c(1)}
    
    # proportion_correct array
    prop_correct <- rep(NA, n_sims)
    
    # for each sample size
    for (j in 2:max_eggs) {
      
      # pre-allocate correct identifications of number of males
      correct <- rep(NA, n_sims)
      
      for (k in 1:n_sims) {
        
        # simulate male contributions to nest
        nest <- sample(x = 1:i, 
                       size = n_eggs, 
                       replace = TRUE,
                       prob = contributions)
        
        # take samples of size k
        samples <- sample(x = nest, 
                          size = j, 
                          replace = FALSE)
        
        # correct allocation of number of males?
        correct[k] <- length(unique(samples)) == i
        
      }
      
      # calculate index in data frame
      index <- (i - 1)*(max_eggs - 1) + j - 1
      
      # stick proportion in data frame
      DF$Proportion_correct[index] <- mean(correct)
    }
    
  }
  
  #### plot results
  
  # color-blind friendly color palette
  colors <- viridis(max_males)
  
  # plot results
  fig1 <- ggplot(DF, aes(x = Sample_size, y = Proportion_correct, 
                         col = as.factor(Males))) +
    geom_path(lwd = 1) +
    labs(col = 'Number \n of Males') +
    scale_color_manual(values = colors) +
    ylab('Proportion Correct') +
    xlab('Eggs Sampled') +
    geom_vline(xintercept = 32, linetype = 2)
  
  # save results to image file
  ggsave(plot = last_plot(), 
         filename = paste(breeding, '_fig1_eggs_to_sample.png', sep = ''),
         path = 'C://Users/vique/Documents/Projects/iliketurtles/figures',
         width = 6, height = 3)
  
  
  # What's our confidence if we sample 32 percent of the eggs?
  library(dplyr)
  DFsamples <- subset(DF, Sample_size %in% c(32, 48, 64, 80, 96))
  newDFsamples <- DFsamples %>% 
    spread(Sample_size, Proportion_correct)

  png(filename = paste('C://Users/vique/Documents/Projects/iliketurtles/figures/', 
                       breeding, '_ss32_conf_table.png', sep = ''), 
      width = 200, height = 200)
  grid.table(newDFsamples)
  dev.off()
  
  output <- list(fig1, newDFsamples)
  
  return(output)
  
}