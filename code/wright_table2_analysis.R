# wright et al. 2012 table 2

# set working directory
setwd("~/Projects/iliketurtles/code")

# load csv
wright <- read.csv("../data/wright_data.csv")

# table of min males
t1 <- table(wright$Min..inferred.fathers)/length(wright$Min..inferred.fathers)
r1 <- as.vector(t1)
r1 <- c(r1, 0)

# table of max males
t2 <- table(wright$Max..inferred.fathers)/length(wright$Max..inferred.fathers)
r2 <- as.vector(t2)

# dataframe
DF <- data.frame(Min = r1, Max = r2)
DF$Avg <- (DF$Min + DF$Max) / 2

# Average number of males contributing to nest
DF$Avg

# check it sums to 1
sum(DF$Avg)

