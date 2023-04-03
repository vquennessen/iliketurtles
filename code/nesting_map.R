# map of nests

# set working directory
setwd("~/Projects/iliketurtles/code")

# load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# load data file
nesting <- read_xlsx(path = '../data/2021_2022_Nesting_Season_3.xlsx')

# subset of nests
nests <- nesting %>%
  filter(TIPO_REG == 'CD')

# plot lat and long (basic)
ggplot(data = nests, 
       aes(x = LONGITUDE, y = LATITUDE, color = `TURTLE ID`)) +
  geom_point()

# attempt 2
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data=world) +
  geom_sf() + 
  coord_sf(
    crs = 5880, # https://epsg.io/5880
    xlim = c(7261985.99, 7501985.99),  # limits are taken from projected bounds
    ylim = c(9559599.73, 9704221.73)  # of EPSG:5880
  )


world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data=world) +
  geom_sf() + 
  coord_sf(
    crs = 3460, # https://epsg.io/3460
    xlim = c(1798028.61, 2337149.40), # limits are taken from projected bounds
    ylim = c(3577110.39, 4504717.19)  # of EPSG:3460
  )


library(sf)
library(tidyverse)
library(ggrepel) 
library(gridExtra) 
library(spdep)
library(rnaturalearthhires)
devtools::install_github("robinlovelace/geocompr")
library(geocompr)

shp_brasil <-  read_sf("datos_espaciales/shp_brasil.shp")
