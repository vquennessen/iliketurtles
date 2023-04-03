# climate estimates

# set working directory
setwd("~/Projects/iliketurtles/code")

# load libraries
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stats)
library(Hmisc)

# load in data
S1 <- read_csv("../data/temperature_season_1.csv")
S2 <- read_csv("../data/temperature_season_2.csv")
S3 <- read_csv("../data/temperature_season_3.csv")

# combine into one dataframe
temps <- rbind(S1, S2, S3)

# set datatypes
temps$Season <- as.factor(temps$Season)
temps$Nest <- as.factor(temps$Nest)
temps$Tinytag <- as.factor(temps$Tinytag)
temps$Time <- as.POSIXct(temps$Time,
                         format = "%m/%d/%Y  %H:%M", 
                         tz = "")
temps$Date <- as.Date(temps$Time)

# add new time columns
temps$Month <- as.factor(month(temps$Time))

temps <- temps %>% 
  group_by(Season, Nest) %>% 
  mutate(Day = Date - first(Date))

# chop off last day and temps over 50 or below 10
temps_cutoff <- temps %>%
  group_by(Season, Nest) %>%
  filter(Day < max(Day)) %>%
  filter(Temperature < 50 & Temperature > 10)

# histograms by month and year
ggplot(data = temps_cutoff, aes(x = Month, y = Temperature, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ Season, ncol = 1)

# only include middle third of incubation time for sex determination
incubation_temps <- temps_cutoff %>%
  group_by(Season, Nest) %>%
  filter(Day > last(Day) / 3) %>%
  filter(Day < 2*last(Day) / 3)

# histograms by month and year
ggplot(data = incubation_temps, 
       aes(x = Month, y = Temperature, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ Season, ncol = 1)

# mean and median values
all_incubation_temps <- incubation_temps %>%
  filter(Season != 1) %>%
  group_by(Month) %>%
  dplyr::summarize(Mean = mean(Temperature), 
                   Median = median(Temperature), 
                   Variance = var(Temperature))

S2_incubation_temps <- incubation_temps %>%
  filter(Season == 2) %>%
  group_by(Month) %>%
  dplyr::summarize(Mean = mean(Temperature), 
                   Median = median(Temperature), 
                   Variance = var(Temperature))

S3_incubation_temps <- incubation_temps %>%
  filter(Season == 3) %>%
  group_by(Month) %>%
  dplyr::summarize(Mean = mean(Temperature), 
                   Median = median(Temperature), 
                   Variance = var(Temperature))

# copy S3 incubation temps for S2 month 6
S2_incubation_temps[6, ] <- S3_incubation_temps[6, ]

# very slight skew, but overall very symmetric - will use median

# import nesting data
load("../data/nests.Rdata")

# determine proportion of nests with incubation temperatures per month

# create function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# determine dates of incubation for nests
nests <- nests %>%
  filter_at(vars(DATA_OCORR, DATA_ECLOS), all_vars(!is.na(.))) %>%
  mutate(Incubation_start = DATA_OCORR + days(floor(as.numeric(date(DATA_ECLOS) - date(DATA_OCORR)) / 3)), 
         Incubation_end = DATA_OCORR + days(ceiling(as.numeric(date(DATA_ECLOS) - date(DATA_OCORR)) * 2 / 3)))

# pull out most common month from each incubation period
for (i in 1:nrow(nests)) {
  
  incubation  <- seq(from = nests$Incubation_start[i], 
                     to = nests$Incubation_end[i], 
                     by = 'days')
  
  months <- format(incubation, format = '%m')
  
  nests$Key_month[i] <- getmode(as.numeric(months))
  
}

# pull out proportion of nests with peak incubation in each month (S2 and S3 only)
all_proportions <- nests %>%
  filter(Season != 2020) %>%
  group_by(Key_month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

S2_proportions <- nests %>%
  filter(Season == 2021) %>%
  group_by(Key_month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

S3_proportions <- nests %>%
  filter(Season == 2022) %>%
  group_by(Key_month) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# quick check that proportions add up to 1 - good to go
# sum(S2_proportions$freq)
# sum(S3_proportions$freq)

# calculate weighted mean for S2 and S3
all_mean_temp <- weighted.mean(x = all_incubation_temps$Mean, 
                               y = all_proportions$freq)

# Means
# 31.54639

# Medians
# 31.4195

# calculate weighted mean for S2
S2_mean_temp <- weighted.mean(x = S2_incubation_temps$Mean, 
                              y = S2_proportions$freq)
# Means
# 32.04623

# Medians
# 31.90867

# calculate weighted mean for S2
S3_mean_temp <- weighted.mean(x = S3_incubation_temps$Mean, 
                              y = S3_proportions$freq)

# Means
# 31.44746

# Medians
# 31.3

# TODO
# calculate weighted standard deviation

# all seasons
all_sd_temp <- sd(incubation_temps$Temperature)

# 1.433039

season_temps <- incubation_temps %>%
  filter(Season != 1) %>%
  ungroup() %>%
  summarise(sd = sd(Temperature))

# 1.42

# season 2
season_temps <- incubation_temps %>%
  group_by(Season) %>%
  summarise(sd = sd(Temperature))

# season 1 - 1.01
# season 2 - 1.31
# season 3 - 1.37

################################################################################

# overall incubation temperatures mean for S2 and S3: 31.54639

# overall incubation temperatures sd for S2 and S3: 1.42
