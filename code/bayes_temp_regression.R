# bayes temp regression to sex ratio

##### set up ###################################################################

# set working directory
setwd("~/Projects/iliketurtles")

# load libraries
library(dplyr)
library(R2jags)
library(mcmcplots)

###### load data ###############################################################

# temperature data
load("data/temperature_data.Rda")

# sex ratio data
patricio_prop_M <- read.csv("data/patricio_prop_male.csv")

##### bayesian model for nest temp based on sea and air temp ###################
sink("LM.jags")

cat("model {
    
      intercept ~ dt(0, pow(1,-2), 1)
      air_slope ~ dt(0, pow(1,-2), 1)
      SST_slope ~ dt(0, pow(1,-2), 1)
      epsilon ~ dunif(0,5)
    
      for(i in 1:n.obs) {
      
        delta_temp[i] = intercept + avg_air_temp[i]*air_slope + avg_SST[i]*SST_slope + Nest[i] + Season[i]
        avg_nest_temp_exp[i] = avg_nest_temp[i-1] + delta_temp[i]
        avg_nest_temp[i] ~ dnorm(avg_nest_temp_exp[i], pow(epsilon, -2))

      } # i
    
    }", fill = TRUE)

sink()

# Parameters monitored
parameters <- c("intercept", "air_slope", "SST_slope", "epsilon")

# MCMC Settings
ni <- 40000
nt <- ni/1000
nb <- ni/2
nc <- 3

# Data
jags.data = list(n.obs = length(temps$avg_air_temp),
                 nest_temp = c(temps$avg_nest_temp),
                 air_temp = c(temps$avg_air_temp), 
                 SST = c(temps$avg_sst))

JagsLM <- jags(jags.data, 
               inits = NULL, 
               parameters, 
               "LM.jags", 
               n.chains = nc, 
               n.thin = nt, 
               n.iter = ni, 
               n.burnin = nb, 
               working.directory = getwd())

print(JagsLM)

attach.jags(JagsLM)

summary(lm(body_mass_g~bill_length_mm,data=penguins_complete))

mcmcplot(JagsLM)

##### bayesian model for nest temp based on sea and air temp ###################
sink("LM.jags")

cat("model {
    
      intercept ~ dt(0, pow(1,-2), 1)
      slope ~ dt(0, pow(1,-2), 1)
      epsilon ~ dunif(0,5)
    
      for(i in 1:n.obs) {
      
        logit_prop_M[i] = intercept + avg_nest_temp[i]*slope
        avg_nest_temp_exp[i] = avg_nest_temp[i-1] + delta_temp[i]
        avg_nest_temp[i] ~ dnorm(avg_nest_temp_exp[i], pow(epsilon, -2))

      } # i
    
    }", fill = TRUE)

sink()

# Parameters monitored
parameters <- c("intercept", "air_slope", "SST_slope", "epsilon")

# MCMC Settings
ni <- 40000
nt <- ni/1000
nb <- ni/2
nc <- 3

# Data
jags.data = list(n.obs = length(temps$avg_air_temp),
                 nest_temp = c(temps$avg_nest_temp),
                 air_temp = c(temps$avg_air_temp), 
                 SST = c(temps$avg_sst))

JagsLM <- jags(jags.data, 
               inits = NULL, 
               parameters, 
               "LM.jags", 
               n.chains = nc, 
               n.thin = nt, 
               n.iter = ni, 
               n.burnin = nb, 
               working.directory = getwd())

print(JagsLM)

attach.jags(JagsLM)

summary(lm(body_mass_g~bill_length_mm,data=penguins_complete))

mcmcplot(JagsLM)



