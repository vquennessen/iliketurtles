# past air temp and SST data

# links
# https://www.quora.com/How-do-I-open-GRIB-files-in-R
# https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html


# set working directory
setwd('~/Projects/iliketurtles')

library(rNOMADS)
library(raster)

# load in pre-2022 data
ReadGrib(file.names = 'data/climate_data_before_2022.grib', 
         file.type = 'grib1')

GRIB <- brick('data/climate_data_before_2022.grib')
GRIB <- brick('data/climate_data_2022.grib')
