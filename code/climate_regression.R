# past air temp and SST data

# links
# https://www.quora.com/How-do-I-open-GRIB-files-in-R
# https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html


# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
# library(rNOMADS)
library(raster)

# load in pre-2022 data
# ReadGrib(file.names = 'data/climate_data_before_2022.grib', 
#          file.type = 'grib1')

# import objects as RasterBricks using raster package
GRIB <- brick('data/climate_data_before_2022.grib')
GRIB2 <- brick('data/climate_data_2022.grib')

# from rasterbrick to dataframe
before2022 <- raster::as.data.frame(GRIB, xy = FALSE, long = TRUE)
during2022 <- raster::as.data.frame(GRIB2, xy = FALSE, long = TRUE)

RTP <- rasterToPoints(GRIB)
