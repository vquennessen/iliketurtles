
yourpath <- file.path(getwd(), "dataOut") # The location of incubtemp_lags.RData on you computer

load(file.path(yourpath, "incubtemp_lags.RData")) # load the file

write.csv(incubtemp, file=file.path(yourpath, "incubtemp_lags.csv")) # export in .csv to the same location as incubtemp_lags.RData

## the object incubtemp is a data.frame with 9 columns:
# 'datetime' dates and times in GMT00 (yyyy-mm-dd HH:MM:SS): Note that you might want to convert those according to the study site's time zone to compare these predictions with temperatures recorded in-situ
# 'time' cumulative time (in minutes) 
# 'present' reconstructed nest temperatures between 1979 and 2021
# 'rcp4.5' future nest temperatures if the rcp4.5 warming scenario is applied (using the median increase in temperature by 2100)
# 'rcp4.5_P25' future nest temperatures if the rcp4.5 warming scenario is applied (using the 25th percentile instead of the median)
# 'rcp4.5_P75' future nest temperatures if the rcp4.5 warming scenario is applied (using the z5th percentile instead of the median)
# 'rcp8.5' future nest temperatures if the rcp8.5 warming scenario is applied (using the median increase in temperature by 2100)
# 'rcp8.5_P25' future nest temperatures if the rcp8.5 warming scenario is applied (using the 25th percentile instead of the median)
# 'rcp8.5_P75' future nest temperatures if the rcp8.5 warming scenario is applied (using the 75th percentile instead of the median)

attr(incubtemp, "timezone") # time zone of in loggers' data

## thermal properties from loggers' data:
attr(incubtemp, "amplitude") # mean daily amplitude (i.e., difference between maxima and minima in degrees Celsius)
attr(incubtemp, "timemin") # mean time of day when minima occurred (in decimal hours after midnight)
attr(incubtemp, "timemax") # mean time of day when maxima occurred (in decimal hours after midnight)

## parameters used to reconstruct long-term nest temperatures (for details, see Monsinjon, J. R., et al. (2019). "The climatic debt of loggerhead sea turtle populations in a warming world." Ecological Indicators 107: 105657. https://doi.org/10.1016/j.ecolind.2019.105657):
attr(incubtemp, "lag_sst") # number of days lagged between mean daily nest temperatures and mean daily sea surface temperatures
attr(incubtemp, "lag_t2m") # number of days lagged between mean daily nest temperatures and mean daily 2-meter air temperatures
attr(incubtemp, "intercept") # intercept coefficient from the GLM analysis
attr(incubtemp, "coef_sst") # coefficient of the relationship between mean daily nest temperatures and mean daily sea surface temperatures
attr(incubtemp, "coef_t2m") # coefficient of the relationship between mean daily nest temperatures and mean daily 2-meter air temperatures
attr(incubtemp, "coef_metabolheat") # average temperature difference between the middle of the clutch and the surrounding sand due to metabolic heating
attr(incubtemp, "sd_rdm") # standard deviation of the coefficients of the GLMM's random effects (used as a proxy of the beach's thermal heterogeneity at nest depth)

## lower and upper deviation of future nest temperatures if predicted from RCP4.5 and RCP8.5's 25th and 75th percentiles (i.e, a measure of the uncertainties in warming scenarios)
attr(incubtemp, "sd_low_rcp4.5")
attr(incubtemp, "sd_up_rcp4.5")
attr(incubtemp, "sd_low_rcp8.5")
attr(incubtemp, "sd_up_rcp8.5")

## IPCC's AR6 warming scenarios (for details on settings, see description below):
# Atlas: https://interactive-atlas.ipcc.ch/regional-information
# Dataset = CMIP6 (Model projections)
# Variable = Mean temperature (T) and Sea Surface Temperature (SST) anomalies (change in deg C)
# Region set = WGI reference-regions (here region "NES" for Fernando de Noronha)
# Uncertainty = Advanced
# Baseline period = 1981-2010
# Future period = 2081-2100
# Season = Annual
# Scenario = SSP2-4.5 and SSP5-8.5 (hereinafter referred as RCP4.5 and RCP8.5 respectively)
# Middle of the road = SSP2-4.5: Approximately in line with the upper end of combined pledges under the Paris Agreement. The scenario “deviates mildly from a ‘no-additional climate-policy’ reference scenario, resulting in a best-estimate warming around 2.7C by the end of the 21st century”.
# Business as usual = SSP5-8.5: A high reference scenario with no additional climate policy. Emissions as high as SSP5-8.5 are only achieved within the fossil-fuelled SSP5 socioeconomic development pathway.
attr(incubtemp, "RCP4.5_air") # median
attr(incubtemp, "RCP4.5_air_P25") # 25th percentile
attr(incubtemp, "RCP4.5_air_P75") # 75th percentile
attr(incubtemp, "RCP8.5_air") # median
attr(incubtemp, "RCP8.5_air_P25") # 25th percentile
attr(incubtemp, "RCP8.5_air_P75") # 75th percentile
attr(incubtemp, "RCP4.5_sea") # median
attr(incubtemp, "RCP4.5_sea_P25") # 25th percentile
attr(incubtemp, "RCP4.5_sea_P75") # 75th percentile
attr(incubtemp, "RCP8.5_sea") # median
attr(incubtemp, "RCP8.5_sea_P25") # 25th percentile
attr(incubtemp, "RCP8.5_sea_P75") # 75th percentile
