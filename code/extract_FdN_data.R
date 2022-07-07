# extract biological parameters from FdN data

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(reshape)

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
  mutate(Female = NA)

# initialise females reference dataframe with column names and first female

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

# overall mean
mean(nests_egg_counts$OVOS_TOT) 
# 102.2625

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

# SD for each season
sd_eggs <- nests %>%
  group_by(Season) %>%
  summarise(SD = sd(OVOS_TOT, na.rm = TRUE))
# season 1: 27.1
# season 2: 18.8
# season 3: 20.9

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

##### number of nests ##########################################################

