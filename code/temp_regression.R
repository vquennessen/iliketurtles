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
library(tictoc)

# load in data
load("~/Projects/iliketurtles/data/temperature_data.Rda")

# ARMA correlation object
ARMA <- corARMA(value = c(0.2, 0.2),                    # just starting value
                form = ~ date,                          # correlation by date
                p = 1, q = 1)                           # for ARMA correlation

# random effect for nest nested within season, AR correlation, no lag
AR_no_lag <- lme(avg_nest_temp ~ avg_air_temp + avg_sst + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = corAR1(form = ~ date))   # AR correlation

# random effect for nest nested within season, ARMA correlation, no lag
ARMA_no_lag <- lme(avg_nest_temp ~ avg_air_temp + avg_sst + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation

# anova
anova(AR_no_lag, ARMA_no_lag)
#             Model df      AIC      BIC    logLik   Test L.Ratio p-value
# AR_no_lag       1  8 13485.54 13538.65 -6734.768                       
# ARMA_no_lag     2  9 13335.37 13395.12 -6658.683 1 vs 2  152.17  <.0001

# random effect for nest nested within season, AR correlation, lag 1
AR_lag1 <- lme(avg_nest_temp ~ airlag1 + sstlag1 + incubation.prop, 
               data = temps,                            # dataframe
               random = ~ 1 | Season / Nest,            # random effect of nest
               correlation = corAR1(form = ~ date))     # AR correlation

# random effect for nest nested within season, ARMA correlation, lag 1
tic()
ARMA_lag1 <- lme(avg_nest_temp ~ airlag1 + sstlag1 + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation
toc() # 11.5 seconds

# anova
anova(AR_lag1, ARMA_lag1)

#            Model df  AIC       BIC       logLik     Test    L.Ratio   p-value
# AR_lag1    1     8   13494.76  13547.88  -6739.381                        
# ARMA_lag1  2     9   13339.30  13399.06  -6660.650  1 vs 2  157.4611  <.0001

# random effect for nest nested within season, AR correlation, lag 2
AR_lag2 <- lme(avg_nest_temp ~ airlag2 + sstlag2 + incubation.prop, 
               data = temps,                            # dataframe
               random = ~ 1 | Season / Nest,            # random effect of nest
               correlation = corAR1(form = ~ date))     # AR correlation

# random effect for nest nested within season, ARMA correlation, lag 2
ARMA_lag2 <- lme(avg_nest_temp ~ airlag2 + sstlag2 + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation

# anova
anova(AR_lag2, ARMA_lag2)

#           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# AR_lag2       1  8 13491.01 13544.13 -6737.506                        
# ARMA_lag2     2  9 13338.97 13398.73 -6660.485 1 vs 2 154.0407  <.0001

# random effect for nest nested within season, AR correlation, lag 3
AR_lag3 <- lme(avg_nest_temp ~ airlag3 + sstlag3 + incubation.prop, 
               data = temps,                            # dataframe
               random = ~ 1 | Season / Nest,            # random effect of nest
               correlation = corAR1(form = ~ date))     # AR correlation

# random effect for nest nested within season, ARMA correlation, lag 3
ARMA_lag3 <- lme(avg_nest_temp ~ airlag3 + sstlag3 + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation

# anova
anova(AR_lag3, ARMA_lag3)

#           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# AR_lag3       1  8 13493.51 13546.62 -6738.753                        
# ARMA_lag3     2  9 13341.75 13401.51 -6661.876 1 vs 2 153.7548  <.0001

# random effect for nest nested within season, AR correlation, lag 4
AR_lag4 <- lme(avg_nest_temp ~ airlag4 + sstlag4 + incubation.prop, 
               data = temps,                            # dataframe
               random = ~ 1 | Season / Nest,            # random effect of nest
               correlation = corAR1(form = ~ date))     # AR correlation

# random effect for nest nested within season, ARMA correlation, lag 4
ARMA_lag4 <- lme(avg_nest_temp ~ airlag4 + sstlag4 + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation

# anova
anova(AR_lag4, ARMA_lag4)

#           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# AR_lag4       1  8 13496.03 13549.15 -6740.014                        
# ARMA_lag4     2  9 13342.57 13402.33 -6662.286 1 vs 2 155.4564  <.0001

# random effect for nest nested within season, AR correlation, lag 5
AR_lag5 <- lme(avg_nest_temp ~ airlag5 + sstlag5 + incubation.prop, 
               data = temps,                            # dataframe
               random = ~ 1 | Season / Nest,            # random effect of nest
               correlation = corAR1(form = ~ date))     # AR correlation

# random effect for nest nested within season, ARMA correlation, lag 5
ARMA_lag5 <- lme(avg_nest_temp ~ airlag5 + sstlag5 + incubation.prop, 
                 data = temps,                          # dataframe
                 random = ~ 1 | Season / Nest,          # random effect of nest
                 correlation = ARMA)                    # ARMA correlation

# anova
anova(AR_lag5, ARMA_lag5)

#           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# AR_lag5       1  8 13491.30 13544.41 -6737.649                        
# ARMA_lag5     2  9 13337.96 13397.72 -6659.981 1 vs 2 155.3351  <.0001

anova(AR_no_lag, AR_lag1, AR_lag2, AR_lag3, AR_lag4, AR_lag5, 
      ARMA_no_lag, ARMA_lag1, ARMA_lag2, ARMA_lag3, ARMA_lag4, ARMA_lag5, 
      test = FALSE)

#             Model df      AIC      BIC    logLik   
# AR_no_lag       1  8 13485.54 13538.65 -6734.768                        
# AR_lag1         2  8 13494.76 13547.88 -6739.381                        
# AR_lag2         3  8 13491.01 13544.13 -6737.506                        
# AR_lag3         4  8 13493.51 13546.62 -6738.753                        
# AR_lag4         5  8 13496.03 13549.15 -6740.014                        
# AR_lag5         6  8 13491.30 13544.41 -6737.649                        
# ARMA_no_lag     7  9 13335.37 13395.12 -6658.683 ****** lowest AIC/BIC/logLik 
# ARMA_lag1       8  9 13339.30 13399.06 -6660.650                        
# ARMA_lag2       9  9 13338.97 13398.73 -6660.485                        
# ARMA_lag3      10  9 13341.75 13401.51 -6661.876                        
# ARMA_lag4      11  9 13342.57 13402.33 -6662.286                        
# ARMA_lag5      12  9 13337.96 13397.72 -6659.981

# plot normalized residuals of AR correlation with lowest AIC - 
# autocorrelation = no good
plot(ACF(AR_no_lag, maxLag = 10, resType = "n"), alpha = 0.01 )

# plot normalized residuals of ARMA correlation with lowest AIC - 
# no autocorrelation = good
plot(ACF(ARMA_no_lag, maxLag = 10, resType = "n"), alpha = 0.01 )

# summary of "best" model tested
summary(ARMA_no_lag)

# Linear mixed-effects model fit by REML
# Data: temps 
# AIC      BIC    logLik
# 13335.37 13395.12 -6658.683
# 
# Random effects:
#   Formula: ~1 | Season
# (Intercept)
# StdDev:    1.174141
# 
# Formula: ~1 | Nest %in% Season
# (Intercept) Residual
# StdDev: 0.0003867721 2.304818
# 
# Correlation Structure: ARMA(1,1)
# Formula: ~date | Season/Nest 
# Parameter estimate(s):
#   Phi1    Theta1 
# 0.9160922 0.2162078 
# Fixed effects:  avg_nest_temp ~ avg_air_temp + avg_sst + incubation.prop 
#                     Value Std.Error   DF   t-value p-value
# (Intercept)     23.078706  3.862587 5553  5.974934  0.0000
# avg_air_temp    -0.033569  0.017667 5553 -1.900163  0.0575
# avg_sst          0.280301  0.133952 5553  2.092554  0.0364
# incubation.prop  1.262259  0.315704 5553  3.998234  0.0001
# Correlation: 
#   (Intr) avg_r_ avg_ss
# avg_air_temp    -0.034              
# avg_sst         -0.975 -0.089       
# incubation.prop  0.035 -0.031 -0.073
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -3.28520337  0.02497059  0.36346594  0.88826404  2.90567405 
# 
# Number of Observations: 5655
# Number of Groups: 
#   Season Nest %in% Season 
# 3               99 