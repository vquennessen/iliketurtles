# temperature regression

# sources
# random effects in nlme: https://biostatmatt.com/archives/2718

# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
library(nlme)
library(ggplot2)

# load in data
load("~/Projects/iliketurtles/data/temperature_data.Rda")

# basic linear regression
mod1 <- lm(nest_temp ~ temp2m + SST, 
           data = temps)

summary(mod1)
# all very significant correlates, r2 of almost zero

# basic linear regression with datetime
mod2 <- lm(nest_temp ~ temp2m + SST + date_time, 
           data = temps)

summary(mod2)
# still very significant coefficients, but still r2 = 0.05
# plot(mod2)

# random effect for s.nest
m1 <- lme(nest_temp ~ temp2m + SST, 
          random = ~ 1 | s.nest,
          data = temps)
summary(m1)
# AIC      BIC     logLik
# 1054109  1054161 -527049.4

# random effect for nest nested within season
m2 <- lme(nest_temp ~ temp2m + SST, 
          data = temps, 
          random = ~ 1 | Season/Nest)
summary(m2)
# AIC      BIC      logLik
# 1054080  1054143  -527034