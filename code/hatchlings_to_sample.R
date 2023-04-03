### function for number of eggs to sample to determine the number of sires

hatchlings_to_sample <- function(n_hatchlings = 100,         # number of eggs per nest
                                 max_hatchlings = 96,  # max hatchlings sampled
                                 max_males = 7,        # max # of M F can mate with
                                 breeding,             # breeding mode
                                 n_sims = 100000,      # number of simulations to run
                                 dom = 0.9,            # prop fertilized by dom male
                                 n_sizes = c(32, 96))  # sample sizes to run      
{
  
  max_hatchlings <- min(max_hatchlings, n_hatchlings) 
  
  
  # pre-allocate data frame
  DF <- data.frame(Males = rep(1:max_males, each = (max_hatchlings - 1)), 
                   Sample_size = rep(2:max_hatchlings, times = max_males), 
                   Proportion_correct = rep(NA, dim = max_males*(max_hatchlings - 1)))
  
  
  # for each number of males that contribute to a nest:
  for (i in 2:max_males) {
    
    # set contributions per males
    if (breeding == 'dominant') {
      MC <- dom
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
    } else if (breeding == 'exponential') {
      MC <- 0.5
      contributions <- 0.5^c(1:(i-1))
      contributions <- c(contributions, contributions[i-1])
    } else if (breeding == 'random') {
      contributions <- rep(1/i, i)
    } else if (breeding == 'flexible_dominant') {
      contributions <- list(c(0.8868, 0.1132), 
                            c(0.4744, 0.3241, 0.2015), 
                            c(0.5485, 0.2508, 0.1509, 0.0499), 
                            c(0.4744, 0.1982, 0.1523, 0.0997, 0.0755))
    }
    
    # proportion_correct array
    prop_correct <- rep(NA, n_sims)
    
    # for each sample size
    for (j in 2:max_hatchlings) {
      
      # pre-allocate correct identifications of number of males
      correct <- rep(NA, n_sims)
      
      for (k in 1:n_sims) {
        
        # simulate male contributions to nest
        if (breeding != 'flexible_dominant') {
          nest <- sample(x = 1:i, 
                         size = n_hatchlings, 
                         replace = TRUE,
                         prob = contributions)
        } else {
          nest <- sample(x = 1:i, 
                         size = n_hatchlings, 
                         replace = TRUE,
                         prob = contributions[[i - 1]])
        }
        
        # take samples of size k
        samples <- sample(x = nest, 
                          size = j, 
                          replace = FALSE)
        
        # correct allocation of number of males?
        correct[k] <- length(unique(samples)) == i
        
      }
      
      # calculate index in data frame
      index <- (i - 1)*(max_hatchlings - 1) + j - 1
      
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
    geom_hline(yintercept = 0.8, linetype = 2) +
    geom_path(lwd = 1) +
    labs(col = 'Number \n of Males') +
    scale_color_manual(values = colors) +
    ylab('Proportion Correct') +
    xlab('Hatchlings Sampled') +
    geom_vline(xintercept = c(n_sizes), linetype = 3)
  
  # save results to image file
  
  if (breeding == 'dominant') {
    ggsave(plot = last_plot(), 
           filename = paste(breeding, '_', dom*100, 
                            '_fig1_hatchlings_to_sample.png', sep = ''),
           path = 'C://Users/Vic/Documents/Projects/iliketurtles/figures',
           width = 6, height = 3)
    
  } else {
    ggsave(plot = last_plot(), 
           filename = paste(breeding, '_fig1_hatchlings_to_sample.png', sep = ''),
           path = 'C://Users/Vic/Documents/Projects/iliketurtles/figures',
           width = 6, height = 3)
  }
  
  # What's our confidence if we sample 32 percent of the eggs?
  library(dplyr)
  DFsamples <- subset(DF, Sample_size %in% n_sizes)
  newDFsamples <- DFsamples %>% 
    spread(Sample_size, Proportion_correct)
  
  
  if (breeding == 'dominant') {
    png(filename = paste('C://Users/Vic/Documents/Projects/iliketurtles/figures/', 
                         breeding, '_', dom*100, '_conf_table.png', sep = ''), 
        width = 200, height = 200)
    grid.table(newDFsamples)
    dev.off()
  } else {
  png(filename = paste('C://Users/Vic/Documents/Projects/iliketurtles/figures/', 
                       breeding, '_conf_table.png', sep = ''), 
      width = 200, height = 200)
  grid.table(newDFsamples)
  dev.off()
  }
  
  output <- list(fig1, newDFsamples)
  
  return(output)
  
}