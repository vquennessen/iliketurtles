# extract and clean up past air temp and SST data

# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
library(ncdf4)
library(dplyr)
library(lubridate)

##### load in and clean up temp data ###########################################

# load in ncdf4 object
nc_data1 <- nc_open('data/adaptor.mars.internal-1665024804.1999784-29758-13-579b4eb1-0631-4829-bf0f-d82db7c690ba.nc')
nc_data2 <- nc_open('data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

# extract time (since lat and long are all the same)
t1 <- ncvar_get(nc_data1, "time")
t2 <- ncvar_get(nc_data2, "time")

# extract air temp 2m above surface of whatever and SST
temp2m1 <- ncvar_get(nc_data1, "t2m") 
temp2m2 <- ncvar_get(nc_data2, "t2m") 
SST1 <- ncvar_get(nc_data1, "sst")
SST2 <- ncvar_get(nc_data2, "sst")

# what is the fillvalue?
temp2m1_fill <- ncatt_get(nc_data1, "t2m", "_FillValue")
temp2m2_fill <- ncatt_get(nc_data2, "t2m", "_FillValue")
SST1_fill <- ncatt_get(nc_data1, "sst", "_FillValue")
SST2_fill <- ncatt_get(nc_data2, "sst", "_FillValue")

# close the netCDF file
nc_close(nc_data1)
nc_close(nc_data2)

# replace fill values with NAs
temp2m1[temp2m2 == temp2m1_fill$value] <- NA
temp2m2[temp2m2 == temp2m2_fill$value] <- NA
SST1[SST1 == SST1_fill$value] <- NA
SST2[SST2 == SST2_fill$value] <- NA

# adjust temperatures to Celsius from Kelvin by subtracting 273.15
temp2m <- c(temp2m1 - 273.15, temp2m2 - 273.15)
SST <- c(SST1 - 273.15, SST2 - 273.15)

# turn time into datetime 
date_time <- c(as.POSIXct(t1*3600, origin = '1900-01-01 00:00'), 
               as.POSIXct(t2*3600, origin = '1900-01-01 00:00'))

# create dataframe with times, temp2ms, and SSTs
weather <- data.frame(Hours = as.character(c(t1, t2)), 
                      date_time = date_time, 
                      temp2m = temp2m, 
                      SST = SST)

weather

##### load in and clean up nesting temp data ###################################

season1 <- read.csv('data/temperature_season_1.csv')
season2 <- read.csv('data/temperature_season_2.csv')
season3 <- read.csv('data/temperature_season_3.csv')

# put 3 seasons of data together
nest_temp <- rbind(season1, season2, season3)

### clean up data

# make time object datetime, and rename as date_time
nest_temp$date_time <- as.POSIXct(nest_temp$Time, format = '%m/%d/%Y  %H:%M')

# rename Temperature column to nest
rename(nest_temp, nest = Temperature)

# add hours column as number of hours since jan 1st 1900 00:00
origin <-  as.POSIXct(mdy_hms('01/01/1900 00:00:00'), format = '%m/%d/%Y %H:%M:%S')
nest_temp$Hours <- difftime(nest_temp$date_time, origin, tz ='UTC', units = "hours")
nest_temp$Hours <- as.character(nest_temp$Hours)

##### join dataframes ##########################################################

# remove duplicated rows in data DF
weather2 <- weather[-c(7354, 16090, 24994), ]

# make season and nest factor variables
nest_temp$Nest <- as.factor(nest_temp$Nest)
nest_temp$Season <- as.factor(nest_temp$Season)

temps <- nest_temp %>%
  left_join(weather2) %>%
  mutate(s.nest = paste(Season, '.', Nest, sep = '')) %>%
  select(date_time, Temperature, temp2m, SST, Nest, s.nest, Season) %>%
  na.omit() %>%
  rename(nest_temp = Temperature) %>%
  group_by(s.nest) %>%
  mutate(incubation.day = as.numeric((date_time - min(date_time)) / (3600*24)))

# save temps as object for further analysis
save(temps, 
     file = 'data/temperature_data.Rda')
