# cumulative density function of a beta distribution

# load libraries
library(ggplot2)

# values of x to plot
x <- seq(from = 0, to = 1, by = 0.01)

# beta values to cycle through
betas <- c(1:5)

# initialise DF
DF <- data.frame(Breeding_Sex_Ratio = rep(x, times = length(betas)), 
                 Beta = rep(betas, each = length(x)), 
                 Reproductive_Success = NA)

# CDF function
for (b in 1:length(betas)) {
  
  start <- (b - 1)*length(x) + 1
  stop <- start + length(x) - 1
  DF$Reproductive_Success[start:stop] <- pbeta(x, shape1 = 1, shape2 = betas[b])
  
}

# make Beta factor
DF$Beta <- as.factor(DF$Beta)

# plot
ggplot(data = DF, aes(x = Breeding_Sex_Ratio, y = Reproductive_Success, 
                      color = Beta)) +
  geom_line(size = 1.25)
