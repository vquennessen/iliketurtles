####### check if results change with different nest sizes ######
### they do NOT ###

# population parameters
max_males      <- 10     # maximum number of males females can mate with

# model parameters
n_sims         <- 1000

# different numbers of eggs
n_eggs <- c(50, 100, 250)

for (e in 1:length(n_eggs)) {

  # pre-allocate data frame
  DF <- data.frame(Males = rep(1:max_males, each = (n_eggs[e] - 1)),
                   Sample_size = rep(2:n_eggs[e], times = max_males),
                   Proportion_correct = rep(NA, dim = max_males*(n_eggs[e] - 1)))

  # for each number of males that contribute to a nest:
  for (i in 1:max_males) {

    # proportion_correct array
    prop_correct <- rep(NA, n_sims)

    # for each sample size
    for (j in 2:n_eggs[e]) {

      # pre-allocate correct identifications of number of males
      correct <- rep(NA, n_sims)

      for (k in 1:n_sims) {

        # simulate male contributions to nest
        nest <- sample(1:i, size = n_eggs[e], replace = TRUE)

        # take samples of size k
        samples <- sample(nest, size = j, replace = FALSE)

        # correct allocation of number of males?
        correct[k] <- length(unique(samples)) == i

      }

      # calculate index in data frame
      index <- (i - 1)*(n_eggs[e] - 1) + j - 1

      # stick proportion in data frame
      DF$Proportion_correct[index] <- mean(correct)
    }

  }

  # add number of eggs column
  DF$Number_of_eggs <- n_eggs[e]

  # save DF as one of three DFs
  if (e == 1) {DF1 <- DF} else if (e == 2) {DF2 <- DF} else {DF3 <- DF}
}

DF <- rbind(DF1, DF2, DF3)

# color-blind friendly color palette
colors <- viridis(max_males)

# plot results
ggplot(DF, aes(x = Sample_size, y = Proportion_correct,
               col = as.factor(Males),
               linetype = as.factor(Number_of_eggs))) +
  geom_path(lwd = 1) +
  labs(col = 'Number \n of Males') +
  scale_color_manual(values = colors)