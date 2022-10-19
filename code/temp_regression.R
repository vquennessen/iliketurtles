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

##### model 1 ##################################################################
# random effect for nest nested within season, no autoregressive correlation
mod1 <- lme(avg_nest_temp ~ avg_air_temp + avg_sst + incubation.prop, 
          data = temps, 
          random = ~ 1 | Season / Nest)
summary(mod1)
# AIC       BIC      logLik
# 20937.92  20984.47 -10461.96


##### model 2 ##################################################################
# just AR model = autoregressive model for temporal correlation
AR <- corAR1(value = c(0.2),              # just starting value
             form = ~ date)          # correlation by date_time

# random effect for nest nested within season
mod2 <- lme(avg_nest_temp ~ avg_air_temp + avg_sst + incubation.prop, 
            data = temps,                              # dataframe
          random = ~ 1 | Season / Nest,                # random effect of nest
          correlation = AR)                            # ARMA correlation
summary(mod2)
# AIC       BIC       logLik
# 13838.23  13891.43  -6911.117

# check AR correlation
ar(temps$avg_nest_temp)

# Coefficients:
#   1        2        3        4        5  
# 1.0279  -0.1626   0.0284  -0.0095   0.0283  
# 
# Order selected 5  sigma^2 estimated as  0.71

##### model 3 ##################################################################
# ARMA model = mixed autoregressive-moving average model (for temporal 
# correlation)

# # ARMA correlation
# ARMA <- corARMA(value = c(0.2, 0.2),         # just starting value
#                 form = ~ date_time,          # correlation by date_time
#                 p = 1, q = 1)                # for ARMA correlation
# 
# # random effect for nest nested within season
# m3 <- lme(nest_temp ~ temp2m + SST + incubation.day, # formula
#           data = temps,                              # dataframe
#           random = ~ 1 | Season / Nest,              # random effect of nest
#           correlation = ARMA)                        # ARMA correlation
# summary(m3)
# # AIC       BIC      logLik
# # 1008863   1008958  -504422.7
# 
# save(m3, file = "data/m3.Rda")
# 
# load(file = "data/m3.Rda")
