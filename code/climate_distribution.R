# climate estimates

# set working directory
setwd("~/Projects/iliketurtles/code")

# load libraries
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

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
incubation_temps %>%
  group_by(Month) %>%
  summarize(Mean = mean(Temperature), 
            Median = median(Temperature))

# very slight skew, but overall very symmetric - will use median

# import nesting data
load("../data/nests.Rdata")

# determine proportion of nests with incubation temperatures per month

# determine dates of incubation for nests
nests <- nests %>%
  filter_at(vars(DATA_OCORR, DATA_ECLOS), all_vars(!is.na(.))) %>%
  # group_by(Season, N_NINHO) %>%
  mutate(Incubation_start = DATA_OCORR + days(floor(as.numeric(date(DATA_ECLOS) - date(DATA_OCORR)) / 3)), 
         Incubation_end = DATA_OCORR + days(ceiling(as.numeric(date(DATA_ECLOS) - date(DATA_OCORR)) * 2 / 3))) %>%

