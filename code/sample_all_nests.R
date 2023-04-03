# function to determine our confidence in BSR estimate given how many nests we 
# sample to robustly estimate the BSR for that year

sample_all_nests <- function(max_n_males = 7, # maximum # of M each F mates with
                             max_n_females = 10, # maximum # of F each M mates with
                             minF = 10, # minimum number of F in 1 breeding year
                             minM = 10, # minimum number of M sampled
                             maxF = 60, # maximum number of F in 1 breeding year
                             maxM = 60, # maximum number of M in 1 breeding year
                             breeding = '', # random, exponential, or dominate
                             nsims = 100000 # number of simulations
) 
{
  
  # equal probability of mating with 1-7 males
  Mprob <- rep(1/max_n_males, times = max_n_males)
  
  mating_mode <- 'polyamory'
  # options are 'polygyny' (males mate with multiple females but females mate 
  # with a single male) and 'polyamory' (both males and females have multiple 
  # partners)
  
  # dimensions
  sampM  <- seq(from = minM, to = maxM, by = 5)
  nsampM <- length(sampM)
  sampF  <- seq(from = minF, to = maxF, by = 5)
  nsampF <- length(sampF)
  
  # pre-allocate data frame for results
  DF <- data.frame(Number_of_males = rep(sampM, each = nsampF),
                   Sample_size = rep(sampF, times = nsampM),
                   Percent_identified = rep(NA, times = nsampF*nsampM),
                   Est_BSR = rep(NA, times = nsampF*nsampM))
  
  # set probabilities of detecting contributing males by breeding scheme
  if (breeding == 'random') 
  { # all males contribute evenly to all nests
    
    probs <- c(1, 1, 1, 1, 1, 0.98, 0.95)
    
  } else if (breeding == 'exponential')
  { # first male is represented in all nests, next M in 1/2 of nests, etc.
    
    probs <- c(1, 1, 1, 0.97, 0.75, 0.34, 0.08)
    
  } else if (breeding == 'dominant') 
  { # one male dominates in all nests, and others appear in only 1
    
    probs <- c(1, 1, 0.93, 0.7, 0.41, 0.19, 0.07)
    
  }
  
  # for each number of potential males
  for (m in 1:length(sampM)) {
    
    # pre-allocate vector of correct number of males identified
    # and breeding sex ratio
    percent <- matrix(NA, nrow = nsims, ncol = nsampF)
    BSR_estimate <- matrix(NA, nrow = nsims, ncol = nsampF)
    
    for (sim in 1:nsims) {
      
      # build pool of potential males to mate with
      Mpool <- rep(1:sampM[m], times = max_n_females)
      
      # simulate breeding - create a matrix of males mated with
      if (mating_mode == 'polygyny') {
        
        mates <- matrix(sample(x = Mpool, 
                               size = maxF, 
                               replace = FALSE), 
                        ncol = 1)
        
      } else if (mating_mode == 'polyamory') {
        
        # how many males does each female mate with
        n_males <- sample(1:max_n_males, 
                          size = maxF, 
                          prob = Mprob, 
                          replace = TRUE)
        
        # initialize mates matrix
        mates <- matrix(NA, nrow = maxF, ncol = max_n_males)
        
        # for each female, pull appropriate number of males from mating pool
        for (i in 1:maxF) {
          j <- n_males[i]
          mates[i, ] <- c(sample(Mpool, size = j), rep(NA, max_n_males - j))
        }
        
      }
      
      # identify total number of contributing males (correct value)
      trueM <- length(unique(as.vector(na.omit(mates))))
      
      # if there are no mates, try again:
      while (trueM == 0) { 
        
        # how many males does each female mate with
        n_males <- sample(1:max_n_males, 
                          size = maxF, 
                          prob = Mprob, 
                          replace = TRUE)
        
        # initialize mates matrix
        mates <- matrix(NA, nrow = maxF, ncol = max_n_males)
        
        # for each female, pull appropriate number of males from mating pool
        for (i in 1:maxF) {
          j <- n_males[i]
          mates[i, ] <- c(sample(Mpool, size = j), rep(NA, max_n_males - j))
        }
        
        # identify total number of contributing males (correct value)
        trueM <- length(unique(as.vector(na.omit(mates))))
        
      }
      
      ##### simulate sampling
      
      # for each sample size of females, from 10 to total number of females 
      # nesting
      for (f in 1:length(sampF)) {
        
        # sample sampF[f] females 
        females <- sample(1:maxF, size = sampF[f])
        
        # initialize empty vector of sampled males
        sampled_males <- c()
        
        for (i in 1:f) {
          # extract all associated males from sampled females
          potential_males <- unique(as.vector(na.omit(mates[females[i], ])))
          
          # number of males extracted
          n_males <- length(potential_males)
          
          # sample from all potential males with exponential probability, 
          # assuming we sample one nest
          observed_males <- potential_males * rbinom(n = n_males, 
                                                     size = 1, 
                                                     prob = probs[1:n_males])
          
          # remove observations of zeros
          observed_males <- observed_males[observed_males != 0]
          
          # append sampled_males for this female to all observed_males vector
          sampled_males <- append(sampled_males, observed_males)
          
        }
        
        # number of sampled males with repeats and NAs taken out
        n_sampled_males <- length(unique(na.omit(sampled_males)))
        
        # determine percent of breeding males identified
        percent[sim, f] <- (n_sampled_males / trueM)
        
        # determine estimated breeding sex ratio
        BSR_estimate[sim, f] <- (n_sampled_males / maxF)
        
      }
      
    }
    
    # add proportion of times the correct number of males was IDd by number of 
    # males and sample size
    
    # take mean of correct columns (sample sizes) to get proportion correct
    percent_identified <- colMeans(percent, na.rm = TRUE)
    
    # take mean of correct columns (sample sizes) to get proportion correct
    avg_BSR_estimate <- colMeans(BSR_estimate, na.rm = TRUE)
    
    # calculate appropriate indices for DF
    index_start <- (m - 1)*nsampF + 1
    index_end <- index_start + nsampF - 1
    
    # store proportion correct values in DF
    DF$Percent_identified[index_start:index_end] <- percent_identified
    
    # store average BSM in DF
    DF$Est_BSR[index_start:index_end] <- avg_BSR_estimate
    
  }
  
  # adjust sample_size to be proportional
  DF$Prop_sampled <- DF$Sample_size / maxF
  
  # add column for proportion correct > 80% TRUE or FALSE
  DF$PC <- DF$Percent_identified >= 0.80
  
  # convert data.frame to data.table and reshape to wide
  DF2 <- dcast(setDT(DF), Number_of_males ~ Prop_sampled, value.var = "PC")
  
  # reverse order of rows to match raster order
  # remove first column
  # convert to matrix and then to raster
  r <- raster(as.matrix(DF2[ , .SD[.N:1, -1]]),
              xmn = 0, xmx = ncol(DF2) - 1, ymn = 0, ymx = ncol(DF2) - 1)
  
  # detect clumps of connected cells of the value TRUE
  # convert raster to polygons
  # dissolve polygons into multi-polygons
  polys <- rasterToPolygons(clump(r), dissolve = TRUE)
  
  # grab coordinates of individual polygons and convert to a data.table
  # use idcol = TRUE to enable grouping of paths when plotting
  d_poly <- rbindlist(lapply(polys@polygons,
                             function(x) as.data.table(x@Polygons[[1]]@coords)),
                      idcol = TRUE)
  
  # plot an outline around each 'patch of significant values' using geom_path
  fig2 <- ggplot(data = DF, aes(x = Prop_sampled,
                                y = Number_of_males)) +
    geom_tile(aes(fill = Percent_identified)) +
    scale_fill_viridis(begin = 0, end = 1, direction = -1, discrete = FALSE,
                       breaks = c(0.4, 0.8), labels = c(0.4, 0.8))  +
    ylab('Number of potentially breeding males') +
    xlab('Proportion of females sampled') +
    labs(fill = 'Breeding \n sex ratio \n correctly \n identified') +
    geom_path(data = d_poly, aes(x = 0.25*x / 3 + 0.75/6, 
                                 y = 5*y + 7.5, 
                                 group = .id),
              size = 1, color = "brown1")
  
  # save results to image file
  ggsave(plot = fig2, filename = paste(breeding, '_correct_BSR.png', sep = ''),
         path = 'C:/Users/vique/Documents/Projects/iliketurtles/figures',
         width = 4, height = 3)
  
  # figure 3 - breeding sex ratio heatmap
  fig3 <- ggplot(data = DF, aes(x = Prop_sampled,
                                y = Number_of_males)) +
    geom_tile(aes(fill = Est_BSR)) +
    scale_fill_viridis(begin = 0, end = 1, direction = -1, discrete = FALSE,
                       breaks = c(0.4, 0.8), labels = c(0.4, 0.8))  +
    ylab('Number of potentially breeding males') +
    xlab('Proportion of females sampled') +
    labs(fill = 'Estimated \n breeding \n sex ratio')
  
  # save results to image file
  ggsave(plot = fig3, filename = paste(breeding, '_BSR_estimate.png', sep = ''),
         path = 'C:/Users/vique/Documents/Projects/iliketurtles/figures',
         width = 4, height = 3)
  
  # include both plots in output
  output <- list(fig2, fig3)
  
  # return output
  return(output)
  
}