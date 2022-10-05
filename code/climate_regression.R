# past air temp and SST data

# set working directory
setwd('~/Projects/iliketurtles')

library(rNOMADS)

# load in pre-2022 data
ReadGrib(file.names = 'data/climate_data_before_2022.grib', 
         file.type = 'grib1')
