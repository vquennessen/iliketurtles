# temperature regression

# sources
# random effects in nlme: https://biostatmatt.com/archives/2718
# correlations in nlme: section 5.3 in Mixed-Effects Models in S and S-PLUS 
#     and http://plantecology.syr.edu/fridley/bio793/mixed2.html

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
m1 <- lme(nest_temp ~ temp2m + SST + incubation.day, 
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

# ARMA model = mixed autoregressive-moving average model (for temporal 
# correlation)

# ARMA correlation
ARMA <- corARMA(value = c(0.2, 0.2),         # just starting value
                form = ~ date_time,          # correlation by date_time
                p = 1, q = 1)                # for ARMA correlation

# random effect for nest nested within season
m3 <- lme(nest_temp ~ temp2m + SST + incubation.day, # formula
          data = temps,                              # dataframe
          random = ~ 1 | Season / Nest,              # random effect of nest
          correlation = ARMA)                        # ARMA correlation   
summary(m3)
# AIC      BIC      logLik
# 1054080  1054143  -527034