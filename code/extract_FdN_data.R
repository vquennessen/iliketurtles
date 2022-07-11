# extract biological parameters from FdN data

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(reshape)

##### load in and clean data ###################################################

# set working directory
setwd('~/Projects/iliketurtles/code')

# import data
S1 <- read.csv("../data/2019_2020_FDN_nesting.csv")                   # season 1
S2 <- read.csv("../data/2020_2021_nesting_Season_2.csv")              # season 2
S3 <- read.csv("../data/2021_2022_Nesting_Season_3.csv")              # season 3

# clean up datasets - only keep columns we want, and add season column
new_S1 <- S1 %>%
  select(TIPO_REG, N_NINHO, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, MARCAS_ENC.1, 
         VIVOS, OVOS_TOT, INCUBAÇAO) %>%
  mutate(Season = 2020, 
         MARCAS_ENC.2 = NA)

new_S2 <- S2 %>%
  rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  rename("INCUBAÇAO" = "Tempo.de.incubação") %>%
  select(TIPO_REG, N_NINHO, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, MARCAS_ENC.1, 
         VIVOS, OVOS_TOT, INCUBAÇAO) %>%
  mutate(Season = 2021, 
         MARCAS_ENC.2 = NA)

new_S3 <- S3 %>%
  rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  rename("INCUBAÇAO" = "Tempo.de.incubação") %>%
  select(TIPO_REG, N_NINHO, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, MARCAS_ENC.1, 
         MARCAS_ENC.2, VIVOS, OVOS_TOT, INCUBAÇAO) %>%
  mutate(Season = 2022)

# put all seasons together
all_seasons <- rbind(new_S1, new_S2, new_S3)

# nests
nests <- all_seasons %>%
  filter(TIPO_REG == 'CD') %>%
  mutate(Female = NA) %>%
  mutate(Hatching_success = VIVOS / OVOS_TOT)

##### initialise females reference DF with column names and first female #######

# first marcas
marcas <- as.character(as.vector(nests[7, c(3, 4, 5, 6, 11)]))

# remove NAs from marcas
marcas_no_NAs <- marcas[!is.na(marcas)]

# initialize females dataframe
females <- data.frame(Female = NA, 
                      Marca1 = NA, Marca2 = NA, Marca3 = NA)
females[1, ] <- c(1, marcas_no_NAs, rep(NA, 3 - length(marcas_no_NAs)))


# troubleshooting - turn warnings into errors
# options(warn = 2)
# turn option back off
options(warn = 1)

# go through nests, and assign females by marcas
for (i in 8:nrow(nests)) {
  
  # extract marcas from dataframe
  marcas <- as.character(as.vector(nests[i, c(3, 4, 5, 6, 11)]))
  
  # if there are values that are not NAs:
  if (sum(!is.na(marcas)) > 0) {
    
    # remove NAs from marcas
    marcas_no_NAs <- marcas[!is.na(marcas)]
    
    # determine if a row in females contains marcas
    present <- females %>% filter_at(vars(Marca1, Marca2, Marca3), 
                                     any_vars(. %in% marcas_no_NAs))   
    
    # if the female is not yet present in the females dataframe: 
    if (sum(!is.na(present)) == 0) {
      
      # add to females dataframe
      females[nrow(females) + 1, ] <- c(nrow(females) + 1, 
                                        marcas_no_NAs, 
                                        rep(NA, 3 - length(marcas_no_NAs)))
      
      # add female number to nests dataframe
      nests$Female[i] <- nrow(females)
      
    } else {
      
      # assign female number to nests dataframe
      nests$Female[i] <- as.numeric(present$Female)
      
    }
    
  }
  
}

# save females object to data
save(females, file = '../data/females.Rdata')

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

