# extract and clean up past air temp and SST data

# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
library(ncdf4)
library(dplyr)
library(lubridate)

##### load in and clean up temp data ###########################################

# # load in ncdf4 object
# nc_data1 <- nc_open('data/adaptor.mars.internal-1665024804.1999784-29758-13-579b4eb1-0631-4829-bf0f-d82db7c690ba.nc')
# nc_data2 <- nc_open('data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

# if on laptop
nc_data1 <- nc_open('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/data/adaptor.mars.internal-1665024804.1999784-29758-13-579b4eb1-0631-4829-bf0f-d82db7c690ba.nc')
nc_data2 <- nc_open('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

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
date_time <- c(as.POSIXct(t1*3600, origin = '1900-01-01 00:00', tz = "UTC"), 
               as.POSIXct(t2*3600, origin = '1900-01-01 00:00', tz = "UTC"))

# create dataframe with times, temp2ms, and SSTs
weather <- data.frame(Hours = as.character(c(t1, t2)), 
                      date_time = date_time, 
                      temp2m = temp2m, 
                      SST = SST)

# weather

##### load in and clean up nesting temp data ###################################

season1 <- read.csv('data/temperature_season_1.csv')
season2 <- read.csv('data/temperature_season_2.csv')
season3 <- read.csv('data/temperature_season_3.csv')

# put 3 seasons of data together
nest_temps <- rbind(season1, season2, season3)

nest_temps$Season_Nest <- paste(nest_temps$Season, '_', nest_temps$Nest, sep = '')

### clean up data

# make time object datetime, and rename as date_time
nest_temps$Time <- round(as.POSIXct(nest_temps$Time, 
                                               format = '%m/%d/%Y  %H:%M'), 
                                    units = 'hours')

# rename Temperature column to nest
nest_temps <- rename(nest_temps, nest_temp = Temperature)
nest_temps <- rename(nest_temps, date_time = Time)

# add hours column as number of hours since jan 1st 1900 00:00
origin <-  as.POSIXct(mdy_hms('01/01/1900 00:00:00'), 
                      format = '%m/%d/%Y %H:%M:%S')
nest_temps$Hours <- round(difftime(nest_temps$date_time, origin, tz ='UTC', 
                                  units = "hours"))
nest_temps$Hours <- as.character(nest_temps$Hours)

##### join dataframes ##########################################################

# remove duplicated rows in data DF
weather2 <- weather[-c(7354, 16090, 24994), ]

# make season and nest character variables
# nest_temp$Nest <- as.factor(nest_temp$Nest)
# nest_temp$Season <- as.factor(nest_temp$Season)

# temporary <- nest_temp %>%
#   left_join(weather2, by = "Hours")

temps_days <- nest_temps %>%
  left_join(weather2, by = 'Hours') %>%
  mutate(date = date(date_time.y)) %>%
  na.omit(date) %>%
  group_by(Season, Nest, date) %>%
  summarize(avg_nest_temp = mean(nest_temp, na.rm = TRUE), 
            avg_air_temp = mean(temp2m, na.rm = TRUE), 
            avg_sst = mean(SST, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Season, Nest) %>%
  mutate(incubation.prop = as.numeric(date - min(date)) /
           max(as.numeric(date - min(date)))) %>%
  ungroup() %>%
  mutate(airlag1 = NA, airlag2 = NA, airlag3 = NA, airlag4 = NA, airlag5 = NA, 
         sstlag1 = NA, sstlag2 = NA, sstlag3 = NA, sstlag4 = NA, sstlag5 = NA,
         Season_Nest = paste(Season, '_', Nest, sep = ''))

# why are date_time.x and date_time.y different?
xx <- temps_days$date_time.x
yy <- temps_days$date_time.y
setdiff(xx, yy)

temps_hours <- nest_temp %>%
  left_join(weather2, by = 'Hours') %>%
  na.omit(date_time.y) %>%
  group_by(Season, Nest) %>%
  summarize(avg_nest_temp = mean(nest_temp, na.rm = TRUE), 
            avg_air_temp = mean(temp2m, na.rm = TRUE), 
            avg_sst = mean(SST, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Season, Nest) %>%
  mutate(incubation.prop = as.numeric(date_time.y - min(date_time.y)) /
           max(as.numeric(date_time.y - min(date_time.y)))) %>%
  ungroup() %>%
  mutate(airlag1 = NA, airlag2 = NA, airlag3 = NA, airlag4 = NA, airlag5 = NA, 
         sstlag1 = NA, sstlag2 = NA, sstlag3 = NA, sstlag4 = NA, sstlag5 = NA,
         Season_Nest = paste(Season, '_', Nest, sep = ''))

# troubleshooting why some had NAs for incubation proportion - some dates as NAs 
# sub3_13 <- nest_temp %>%
#   left_join(weather2) %>%
#   mutate(date = date(date_time)) %>%
#   na.omit() %>%
#   group_by(Season, Nest, date) %>%
#   summarize(avg_nest_temp = mean(Temperature, na.rm = TRUE), 
#             avg_air_temp = mean(temp2m, na.rm = TRUE), 
#             avg_sst = mean(SST, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(Season, Nest) %>%
#   filter(Season == 3 & Nest == 13) %>%
#   mutate(incubation.prop = as.numeric(date - min(date)) /
#            max(as.numeric(date - min(date))))

# add in lags
for (i in 6:nrow(temps)) {
  
  # air temp lags
  temps_days$airlag1[i] <- temps_days$avg_air_temp[i - 1]
  temps_days$airlag2[i] <- temps_days$avg_air_temp[i - 2]
  temps_days$airlag3[i] <- temps_days$avg_air_temp[i - 3]
  temps_days$airlag4[i] <- temps_days$avg_air_temp[i - 4]
  temps_days$airlag5[i] <- temps_days$avg_air_temp[i - 5]
  
  # SST lags
  temps_days$sstlag1[i] <- temps_days$avg_sst[i - 1]
  temps_days$sstlag2[i] <- temps_days$avg_sst[i - 2]
  temps_days$sstlag3[i] <- temps_days$avg_sst[i - 3]
  temps_days$sstlag4[i] <- temps_days$avg_sst[i - 4]
  temps_days$sstlag5[i] <- temps_days$avg_sst[i - 5]
  
}

# remove rows with NAs 
# temps <- temps %>% na.omit()

x <- unique(nest_temp$Season_Nest) # 219 nests total
y <- unique(temps$Season_Nest)     # 212 nests total

# setdiff(x, y)
# "1_21"  "2_12"  "2_13"  "2_14"  "2_16"  "3_138" "3_212"

# save temps as object for further analysis
save(temps, 
     file = 'data/temperature_data.Rda')
