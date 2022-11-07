# modified from ../Monsinjon/Cm_Atlantic_Southwest_PauloLara/embryogrowth.R

# set working directory
setwd("~/Projects/iliketurtles")

# load libraries
library(embryogrowth)
library(dplyr)

##### load nest temperatures ###################################################

# 1. load data
load("data/nests.Rdata")

NestInfos <- nests %>%
  select(Season, N_NINHO, TEMP_LOGGER_ID, DATA_OCORR, DATA_ECLOS, OVOS_TOT,
         VIVOS, Hatching_success) %>%
  

NestInfos <- read.csv("Monsinjon/Cm_Atlantic_Southwest_PauloLara/dataIn/NestInformation.csv")

NestInfos$Date.Nesting <- as.character(as.Date(NestInfos$Date.Nesting, format="%d-%b-%y")) # laying dates
NestInfos$Date.Emersion <- as.character(as.Date(NestInfos$Date.Emersion, format="%d-%b-%y")) # hatching dates

NestID <- NestInfos$Nest.Code 

data.files <- list.files(file.path("dataIn", "Nest temperature in situ")) 

label.issues <- which(is.na(match(paste0(NestID, ".csv"), data.files))) 

# 2. format temperatures and dates/time

NestTemps <- list() # database to fill up with formatted temperature data

timezone <- "America/Fortaleza" # https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

for(k in seq_along(NestID)){
  
  data <- read.csv(file.path("dataIn", "Nest temperature in situ", paste0(NestID[k], ".csv")))
  
  if(k==5 | k==6){
    
    # retrieve datetime and temperatures
    temperature <- as.numeric(gsub("Â°C", "", data[, 4]))
    datetime <- as.character(paste0(data[,2], " ", data[,3]))
    
    datetime <- gsub("202  ", "202", datetime)
    datetime <- gsub("202 ", "202", datetime)
    datetime <- gsub("  ", " ", datetime)
    
    # format local time 
    datetime1 <- as.POSIXct(datetime, format="%d/%m/%Y %I:%M:%S %p", tz=timezone)
    datetime2 <- as.POSIXct(datetime, format="%m/%d/%Y %I:%M:%S %p", tz=timezone)
    
    if(k==5){
     
      ind_monthfirst <- which(substr(datetime, 1, 4)=="1/10")[1]
      
      datetime1[ind_monthfirst:length(datetime)] <- datetime2[ind_monthfirst:length(datetime)]
      
      datetime <- datetime1
      
    } else {
      
      ind_monthfirst <- which(substr(datetime, 1, 2)=="1/" | substr(datetime, 1, 2)=="2/")
      
      datetime1[ind_monthfirst] <- datetime2[ind_monthfirst]
      
      datetime <- datetime1
      
    }
    
  } else {
    
    # retrieve datetime and temperatures
    temperature <- as.numeric(gsub("Â°C", "", data[, 4]))
    datetime <- as.character(paste0(data[,2], " ", data[,3]))
    
    # format local time 
    datetime <- as.POSIXct(datetime, format="%d/%m/%Y %I:%M:%S %p", tz=timezone)
    
  }
  
  
  df <- data.frame(temperature=temperature, 
                   datetime=as.character(datetime), 
                   stringsAsFactors=FALSE)
  
  # get laying and hatching dates for this nest
  laying.date <- as.POSIXct(NestInfos$Date.Nesting[k], tz=timezone)
  hatching.date <- as.POSIXct(NestInfos$Date.Emersion[k], tz=timezone)
  
  # if laying and hatching dates are provided, I make sure we keep temperatures that were recorded between those dates
  if(!is.na(laying.date) & !is.na(hatching.date)){
    
    data.trimmed <- which(as.POSIXct(df$datetime, tz=timezone)>=as.POSIXct(laying.date, tz=timezone) & as.POSIXct(df$datetime, tz=timezone)<=as.POSIXct(hatching.date, tz=timezone))
    
    df <- df[data.trimmed, ]
    
  } else {
    
    print(paste0("Temperatures were not proccessed for nest ", NestID[k], " because laying or hatching dates were not available"))
    
  }
  
  plot(as.POSIXct(df$datetime, tz=timezone), df$temperature, type="l", main=NestID[k]) # check if everything looks fine
  
  # check if some temperature data or dates are missing (if so, omit)
  if(any(is.na(df$temperature)) | any(is.na(df$datetime))){
    
    df <- na.omit(df)
    
    print(paste0("Some temperatures or dates are missing for nest ", NestID[k]))
    
  }
  
  # check if temperatures are recorded hourly (if not, compute hourly average)
  timestep <- min(diff(as.numeric(as.POSIXct(df$datetime, tz=timezone)))) # minimum time difference in seconds between to records
  
  if(timestep<60*60){
    
    breaks <- seq(as.POSIXct(substr(df$datetime[1], 1, 10), tz=timezone), as.POSIXct(substr(tail(df$datetime, n=1), 1, 10), tz=timezone), by="1 hours")
    
    hourly <- aggregate(temperature ~ cut(as.POSIXct(df$datetime, tz=timezone), breaks=breaks), data = df, FUN="mean")
    
    df <- data.frame(temperature=hourly$temperature, 
                     datetime=as.character(hourly[,1]), 
                     stringsAsFactors=FALSE)
    
  }
  
  df <- cbind(df, data.frame(datetime_UTC=as.character(convert.tz(as.POSIXct(df$datetime, tz=timezone), "UTC")), # convert local time to greenwich meridian time
                             time=(as.numeric(as.POSIXct(df$datetime, tz=timezone))-as.numeric(as.POSIXct(df$datetime[1], tz=timezone)))/60, # time since laying (make sure it is in minutes) 
                             stringsAsFactors=FALSE))
  
  NestTemps[[k]] <- df
  names(NestTemps)[k] <- NestID[k]
  
}

inctime <- NULL

for(k in seq_along(NestTemps))
  inctime <- rbind(inctime, data.frame(id=names(NestTemps)[k], time=as.numeric(as.POSIXct(tail(NestTemps[[k]][,"datetime"], n=1))-as.POSIXct(NestTemps[[k]][1,"datetime"]))))

tooshort <- which(inctime$time<40)

if(length(tooshort)>0){
  print(names(NestTemps)[tooshort])
  NestTemps <- NestTemps[-tooshort] 
}

save(NestTemps, timezone, file=file.path("dataOut", "NestTemperatures.RData"))
##

##
load(file.path("dataOut", "NestTemperatures.RData"))

inctime <- NULL

for(k in seq_along(NestTemps))
  inctime <- rbind(inctime, data.frame(id=names(NestTemps)[k], time=as.numeric(as.POSIXct(tail(NestTemps[[k]][,"datetime"], n=1))-as.POSIXct(NestTemps[[k]][1,"datetime"]))))

hist(inctime$time)

for(k in seq_along(NestTemps))  
  plot(as.POSIXlt(NestTemps[[k]][,"datetime"]), NestTemps[[k]][,"temperature"], type="l", main=names(NestTemps)[k])

x <- which(names(NestTemps)=="12")

NestTemps <- NestTemps[-x]

x <- which(names(NestTemps)=="20")

temp <- NestTemps[[x]]
ind <- which(temp$temperature<30)
ind <- ind[length(ind)]
temp <- temp[(ind+1):length(temp$datetime),]
temp$time <- as.numeric(as.POSIXlt(temp$datetime)-as.POSIXlt(temp$datetime[1]))/60

NestTemps[[x]] <- temp

x <- which(names(NestTemps)=="21")

temp <- NestTemps[[x]]
ind <- which(temp$temperature<29.5)
ind <- ind[length(ind)]
temp <- temp[(ind+1):length(temp$datetime),]
temp$time <- as.numeric(as.POSIXlt(temp$datetime)-as.POSIXlt(temp$datetime[1]))/60

NestTemps[[x]] <- temp

x <- which(names(NestTemps)=="27")

temp <- NestTemps[[x]]
ind <- which(temp$temperature<30)
ind <- ind[length(ind)]
temp <- temp[(ind+1):length(temp$datetime),]
temp$time <- as.numeric(as.POSIXlt(temp$datetime)-as.POSIXlt(temp$datetime[1]))/60

NestTemps[[x]] <- temp

save(NestTemps, timezone, file=file.path("dataOut", "NestTemperatures.RData"))
