# construct females dataframe

# extract biological parameters from FdN data

# load libraries
library(dplyr)
library(readr)
library(reshape)
library(lubridate)

##### load in and clean data ###################################################

# set working directory
setwd('~/Projects/iliketurtles/code')

# import data
S1 <- read.csv("../data/2019_2020_FDN_nesting.csv")                   # season 1
S2 <- read.csv("../data/2020_2021_nesting_Season_2.csv")              # season 2
S3 <- read.csv("../data/2021_2022_Nesting_Season_3.csv")              # season 3

# clean up datasets - only keep columns we want, and add season column
new_S1 <- S1 %>%
  select(DATA_OCORR, DATA_ECLOS, TIPO_REG, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, 
         MARCAS_ENC.1, VIVOS, OVOS_TOT, COMP_CASCO, LARG_CASCO, N_NINHO) %>%
  mutate(Season = 2020, 
         MARCAS_ENC.2 = NA)

new_S2 <- S2 %>%
  dplyr::rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  select(DATA_OCORR, DATA_ECLOS, TIPO_REG, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, 
         MARCAS_ENC.1, VIVOS, OVOS_TOT, COMP_CASCO, LARG_CASCO, N_NINHO) %>%
  mutate(Season = 2021, 
         MARCAS_ENC.2 = NA)

new_S3 <- S3 %>%
  dplyr::rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  select(DATA_OCORR, DATA_ECLOS, TIPO_REG, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, 
         MARCAS_ENC.1, MARCAS_ENC.2, VIVOS, OVOS_TOT, COMP_CASCO, LARG_CASCO, 
         N_NINHO) %>%
  mutate(Season = 2022)

# put all seasons together
all_seasons <- rbind(new_S1, new_S2, new_S3)

# nests
nests <- all_seasons %>%
  filter(TIPO_REG == 'CD') %>%
  mutate(Female = NA) %>%
  mutate(Hatching_success = VIVOS / OVOS_TOT)

##### initialize females reference DF with column names and first female #######

# first marcas
marcas <- as.character(as.vector(nests[7, c("MARCAS_COL", "MARCAS_COL.1", 
                                            "MARCAS_ENC", "MARCAS_ENC.1", 
                                            "MARCAS_ENC.2")]))

# remove NAs from marcas
marcas_no_NAs <- marcas[!is.na(marcas)]

# initialize females dataframe
females <- data.frame(Female = NA, 
                      Marca1 = NA, Marca2 = NA, Marca3 = NA, 
                      CA_Length = NA, CA_Width = NA)
females[1, ] <- c(1, marcas_no_NAs, rep(NA, 5 - length(marcas_no_NAs)))


# troubleshooting - turn warnings into errors
# options(warn = 2)
# turn option back off
options(warn = 1)

# go through nests, and assign females by marcas
for (i in 8:nrow(nests)) {
  
  # extract marcas from dataframe
  # first marcas
  marcas <- as.character(as.vector(nests[i, c("MARCAS_COL", "MARCAS_COL.1", 
                                              "MARCAS_ENC", "MARCAS_ENC.1", 
                                              "MARCAS_ENC.2")]))  
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
                                        rep(NA, 5 - length(marcas_no_NAs)))
      
      # add female number to nests dataframe
      nests$Female[i] <- nrow(females)
      
    } else {
      
      # assign female number to nests dataframe
      nests$Female[i] <- as.numeric(present$Female)
      
    }
    
  }
  
}

# make columns correct formats
females$Female <- as.factor(females$Female)
females$Marca1 <- as.factor(females$Marca1)
females$Marca2 <- as.factor(females$Marca2)
females$Marca3 <- as.factor(females$Marca3)

nests$DATA_OCORR <- as.POSIXct(nests$DATA_OCORR, format = "%d-%b-%Y", tz = "")
nests$DATA_ECLOS <- as.POSIXct(nests$DATA_ECLOS, format = "%d-%b-%Y", tz = "")
nests$Season <- as.factor(nests$Season)
nests$Female <- as.factor(nests$Female)
nests$N_NINHO <- as.factor(nests$N_NINHO)

# save females object to data
save(females, file = '../data/females.Rdata')

# save nests object to data
save(nests, file = '../data/nests.Rdata')
