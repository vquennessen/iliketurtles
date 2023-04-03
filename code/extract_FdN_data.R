# extract biological parameters from FdN data

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(reshape)

##### load in and clean data ###################################################

# set working directory
setwd('~/Projects/iliketurtles/code')

load('../data/females.Rdata')
load('../data/nests.Rdata')

##### eggs per nest ############################################################

# remove nests with no total egg counts
nests_egg_counts <- nests %>%
  filter(!is.na(OVOS_TOT)) 

# scatterplot of eggs per nest colored by season
ggplot(data = nests_egg_counts, aes(x = as.factor(Season), 
                                    y = OVOS_TOT)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Total eggs per nest \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nests_egg_counts, aes(x = OVOS_TOT, 
                                    fill = as.factor(Season), 
                                    alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Eggs per nest', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nests_egg_counts$OVOS_TOT) 
# 102.2625

# overall median
median(nests_egg_counts$OVOS_TOT) 
# 101.5

# overall SD
sd(nests_egg_counts$OVOS_TOT) 
# 21.42993

# average for each season
mean_eggs <- nests %>%
  group_by(Season) %>%
  summarise(Mean = mean(OVOS_TOT, na.rm = TRUE))
# season 1: 101
# season 2: 101
# season 3: 103

# median for each season
median_eggs <- nests %>%
  group_by(Season) %>%
  summarise(Median = median(OVOS_TOT, na.rm = TRUE))
# season 1: 98
# season 2: 101
# season 3: 103

# SD for each season
sd_eggs <- nests %>%
  group_by(Season) %>%
  summarise(SD = sd(OVOS_TOT, na.rm = TRUE))
# season 1: 27.1
# season 2: 18.8
# season 3: 20.9

# SD and mean for all seasons minus season one
eggs_S2_S3 <- nests %>%
  filter(Season != 2020) 

sd(eggs_S2_S3$OVOS_TOT, na.rm = TRUE)
# 20.1344

mean(eggs_S2_S3$OVOS_TOT, na.rm = TRUE)
# 102.4625

##### hatching success #########################################################

# remove nests with no hatching success calculated
nests_HS <- nests %>%
  filter(!is.na(Hatching_success)) 

# scatterplot of eggs per nest colored by season
ggplot(data = nests_HS, aes(x = as.factor(Season), 
                            y = Hatching_success)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Hatching success \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nests_HS, aes(x = Hatching_success, 
                            fill = as.factor(Season), 
                            alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Hatching Success', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nests_egg_counts$Hatching_success) 
# 0.8150182

# overall median
median(nests_egg_counts$Hatching_success) 
# 0.8849624

# overall SD
sd(nests_egg_counts$Hatching_success) 
# 0.191776

# average for each season
mean_HS <- nests %>%
  group_by(Season) %>%
  summarise(Mean = mean(Hatching_success, na.rm = TRUE))
# season 1: 0.835
# season 2: 0.782
# season 3: 0.829

# median for each season
median_HS <- nests %>%
  group_by(Season) %>%
  summarise(Median = median(Hatching_success, na.rm = TRUE))
# season 1: 0.86 
# season 2: 0.847
# season 3: 0.903

# SD for each season
sd_HS <- nests %>%
  group_by(Season) %>%
  summarise(SD = sd(Hatching_success, na.rm = TRUE))
# season 1: 0.119
# season 2: 0.185
# season 3: 0.212

# SD for all seasons minus season one
sd(HS_S2_S3$Hatching_success, na.rm = TRUE)
# 0.2032503

# mean for all seasons minus season one
HS_S2_S3 <- nests %>%
  filter(Season != 2020) 
mu <- mean(HS_S2_S3$Hatching_success, na.rm = TRUE)
# 0.8110467

# variance for all seasons minus season one
sigma_sq <- var(HS_S2_S3$Hatching_success, na.rm = TRUE)
# 0.04131066

# calculate beta distribution parameters
alpha <- mu*((mu*(1 - mu))/sigma_sq - 1)
alpha
beta <- alpha / mu - alpha
beta



##### number of nests ##########################################################

# nests with females ID'd
nests_females <- nests %>%
  filter(!is.na(Female))

# number of nests for each female, including those where eggs were not counted
nest_counts <- count(nests_females, Female, Season)

# scatterplot of number of nests vs. female number
ggplot(data = nest_counts, aes(x = Female, y = n, col = as.factor(Season))) +
  geom_point() + 
  labs(y = 'Number of nests', 
       color = 'Season')

# excluding first season
nests_females_noS1 <- nests %>%
  filter(!is.na(Female)) %>%
  filter(Season != 2020)

# number of nests
nest_counts_noS1 <- count(nests_females_noS1, Female)

# scatterplot of number of nests vs. female number without Season 1
ggplot(data = nest_counts_noS1, aes(x = Female, y = n)) +
  geom_point()

# boxplot by season
# scatterplot of eggs per nest colored by season
ggplot(data = nest_counts, aes(x = as.factor(Season), y = n)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.4) + 
  geom_boxplot(alpha = 0.5, 
               aes(fill = as.factor(Season))) +
  labs(x = '\n Season', 
       y = 'Number of nests \n') + 
  theme(legend.position = 'none')

# overlapping density plot, colored by season 
ggplot(data = nest_counts, aes(x = n, fill = as.factor(Season), alpha = 0.5)) + 
  geom_density(aes(group = as.factor(Season), 
                   alpha = 0.5)) +
  guides(alpha = 'none') +
  labs(x = '\n Number of nests', 
       y = 'Density \n',
       fill = 'Season')

# overall mean
mean(nest_counts$n) 
# 4.464912

# overall median
median(nest_counts$n) 
# 5

# overall SD
sd(nest_counts$n) 
# 2.053186

# average for each season
mean_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(Mean = mean(n, na.rm = TRUE))
# season 1: 3.73
# season 2: 4.36
# season 3: 5.34

# median for each season
median_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(Median = median(n, na.rm = TRUE))
# season 1: 3
# season 2: 5
# season 3: 5

# SD for each season
sd_nests <- nest_counts %>%
  group_by(Season) %>%
  summarise(SD = sd(n, na.rm = TRUE))
# season 1: 2.02
# season 2: 1.81
# season 3: 1.96

# SD for all seasons minus season one
nests_S2_S3 <- nest_counts %>%
  filter(Season != 2020) 
sd(nests_S2_S3$n, na.rm = TRUE)
# 1.94

# mean for all seasons minus season one
mean(nests_S2_S3$n, na.rm = TRUE)
# 4.942029

# median for all seasons minus season one
median(nests_S2_S3$n, na.rm = TRUE)
# 5

##### remigration interval for females #########################################

# for each female
female_seasons <- nests_females %>%
  select(Female, Season) %>%
  group_by(Female) %>%
  mutate(Seasons = list(unique(Season)))

##### number of males captured #################################################

male_captures <- read.csv('../data/2021_2022_in_Water.xlsx - Turtle capture.csv')

# num_males <- nrow(subset(male_captures, Tag.status == 'New recruit'))
# num_males

# take 2

tag1 <- gsub('BRA ', '', male_captures$Flipper_Tag_.1)
tag1.1 <- as.numeric(gsub('BR ', '', tag1))

tag2 <- gsub('BRA ', '', male_captures$Flipper_Tag_.2)
tag2.1 <- as.numeric(gsub('BR ', '', tag2))

sums <- tag1.1 + tag2.1
length(sums)
length(unique(sums))

# 33 unique males

# double check
new_males <- data.frame(Tag1 = male_captures$Flipper_Tag_.1, 
                        Tag2 = male_captures$Flipper_Tag_.2, 
                        sum = sums)

##### hatchling production #####################################################

load('../data/nests.Rdata')

hatchlings <- nests %>%
  #filter(Season != 2020) %>%
  group_by(Season) %>%
  summarize(nests = n(), 
            eggs = sum(OVOS_TOT, na.rm = TRUE),
            hatchlings = sum(VIVOS, na.rm = TRUE)) %>%
  mutate(avg_hatchlings = nests*102*0.811)

# mean hatchlings = 14642, 10423, 18530


##### number of nests destroyed / predated by season ###########################

noS1 <- nests %>%
  filter(Season != 2020) %>%
  group_by(Season, HIST_NINHO) %>%
  summarize(n = n())
