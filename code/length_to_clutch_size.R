# does CCL correlate to clutch size?

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
  select(TIPO_REG, OVOS_TOT, COMP_CASCO, LARG_CASCO) %>%
  mutate(Season = 2020, 
         MARCAS_ENC.2 = NA)

new_S2 <- S2 %>%
  dplyr::rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  dplyr::rename("INCUBAÇAO" = "Tempo.de.incubação") %>%
  select(TIPO_REG, N_NINHO, MARCAS_COL, MARCAS_COL.1, MARCAS_ENC, MARCAS_ENC.1, 
         VIVOS, OVOS_TOT, INCUBAÇAO) %>%
  mutate(Season = 2021, 
         MARCAS_ENC.2 = NA)

new_S3 <- S3 %>%
  dplyr::rename("TIPO_REG" = "TIPO_REG..ND...Não.determinado..SD...Sem.Desova..ML...Meia.Lua..CD.com.desova.") %>%
  dplyr::rename("INCUBAÇAO" = "Tempo.de.incubação") %>%
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