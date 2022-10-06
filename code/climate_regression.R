# past air temp and SST data

# links
# https://www.quora.com/How-do-I-open-GRIB-files-in-R
# https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html


# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
# library(rNOMADS)
library(raster)
library(ncdf4)
library(rgdal)
library(ggplot2)

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

# load in ncdf4 object
nc_data1 <- nc_open('')
nc_data2 <- nc_open('data/adaptor.mars.internal-1665013288.9338732-20330-9-4f7bf825-ebce-4709-9351-a14d16742749.nc')

# {
#   sink('gimms3g_ndvi_1982-2012_metadata.txt')
#   print(nc_data)
#   sink()
# }

# set variables
# lon <- ncvar_get(nc_data, "longitude")
# lat <- ncvar_get(nc_data, "latitude", verbose = F)
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

# create dataframe with times, temp2ms, and SSTs
data <- data.frame(time = c(t1, t2), 
                   temp2m = c(temp2m1, temp2m2), 
                   SST = c(SST1, SST2))

# adjust temperatures to Celsius from Kelvin by adding 273.15
data$temp2m <- data$temp2m + 273.15
data$SST <- data$SST + 273.15


