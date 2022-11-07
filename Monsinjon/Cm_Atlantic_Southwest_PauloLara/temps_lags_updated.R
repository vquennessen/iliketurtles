
library(nlme)
library(HelpersMG)

load(file.path("dataOut", "NestTemperatures.RData"))
load(file.path("dataIn", "ERA5.RData"))

breaks_forMean <- seq(as.POSIXlt("1978-01-01 00:00:00", tz=timezone), as.POSIXlt("2022-01-01 00:00:00", tz=timezone), "1 days")

hrsaftermidnight_max <- 15
hrsaftermidnight_min <- 3

breaks_forMax <- breaks_forMean+hrsaftermidnight_max*60*60
breaks_forMin <- breaks_forMean+hrsaftermidnight_min*60*60

ERA5$datetime <- convert.tz(as.POSIXlt(ERA5$datetime, tz="UTC"), tz=timezone)

SST <- aggregate(sst ~ cut(datetime, breaks=breaks_forMean), data = ERA5, FUN = function(y){mean(y)})
T2M <- aggregate(t2m ~ cut(datetime, breaks=breaks_forMean), data = ERA5, FUN = function(y){mean(y)})

climate <- data.frame(date=substr(SST[,1], 1, 10), sst=SST[,2], t2m=T2M[,2])

NestTemps <- lapply(NestTemps, function(x){cbind(x, data.frame(date=as.POSIXlt(x[,"datetime"], tz=timezone)))})

Mean <- lapply(NestTemps, function(x){aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMean), data = x, FUN = function(y){mean(y, na.rm = TRUE)})})
Max <- lapply(NestTemps, function(x){aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMax), data = x, FUN = function(y){cbind(max(y, na.rm = TRUE), which.max(y))})})
Min <- lapply(NestTemps, function(x){aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMin), data = x, FUN = function(y){cbind(min(y, na.rm = TRUE), which.min(y))})})

Max <- lapply(NestTemps, function(x){
  aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMax), data = x, 
            FUN = function(y){
              wm <- which(max(y) == y)
              if (length(wm)==1) {
                cbind(max(y, na.rm = TRUE), length(wm), mean(wm), 0)
              } else {
                cbind(max(y, na.rm = TRUE), length(wm), mean(wm), sd(wm))
              }
            }
  )
}
)

Min <- lapply(NestTemps, function(x){
  aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMin), data = x, 
            FUN = function(y){
              wm <- which(min(y) == y)
              if (length(wm)==1) {
                cbind(min(y, na.rm = TRUE), length(wm), mean(wm), 0)
              } else {
                cbind(min(y, na.rm = TRUE), length(wm), mean(wm), sd(wm))
              }
            }
  )
}
)

plot(x = c(0, 48), y=c(1, 450), type="n", bty="n", las=1, xlab="Time for Max", ylab="Number") 

time_max <- NULL
cpt <- 1
for (i in seq_along(NestTemps)) {
  x <- NestTemps[[i]]
  y <- aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMax), data = x, FUN = function(x) x)[[2]]
  for (j in seq_along(y)) {
    wm <- which(y[[j]]==max(y[[j]]))
    points(x=wm+hrsaftermidnight_max-1, y=rep(cpt, length(wm)), pch=".")
    time_max <- c(time_max, wm+hrsaftermidnight_max-1)
    cpt <- cpt + 1
  }
}

hist(time_max)
meantimemax <- mean(time_max)
meantimemax <- ifelse(meantimemax>=24, meantimemax-24, meantimemax)

print(paste0("The time for maxima is ", specify_decimal(meantimemax)))

plot(x = c(0, 48), y=c(1, 450), type="n", bty="n", las=1, xlab="Time for Min", ylab="Number") 

time_min <- NULL
cpt <- 1
for (i in seq_along(NestTemps)) {
  x <- NestTemps[[i]]
  y <- aggregate(temperature ~ cut(x[,"date"], breaks=breaks_forMin), data = x, FUN = function(x) x)[[2]]
  for (j in seq_along(y)) {
    wm <- which(y[[j]]==min(y[[j]]))
    points(x=wm+hrsaftermidnight_min-1, y=rep(cpt, length(wm)), pch=".")
    time_min <- c(time_min, wm+hrsaftermidnight_min-1)
    cpt <- cpt + 1
  }
}

hist(time_min)
meantimemin <- mean(time_min)
meantimemin <- ifelse(meantimemin>=24, meantimemin-24, meantimemin)

print(paste0("The time for minima is ", specify_decimal(meantimemin)))

data <- NULL

lagmax <- 5

for(k in seq_along(NestTemps)){
  
  lgth <- max(c(dim(Mean[[k]])[1], dim(Max[[k]])[1], dim(Min[[k]])[1]))
  
  df <- data.frame(date=rep(NA, lgth), mean=NA, max=NA, min=NA, amplitude=NA, metabolheat=0, logger=names(NestTemps)[k])
  
  df$date[1:dim(Mean[[k]])[1]] <- substr(Mean[[k]][,1], 1, 10)
  df$mean[1:dim(Mean[[k]])[1]] <- Mean[[k]][,2]
  df$max[1:dim(Max[[k]])[1]] <- Max[[k]][[2]][,1]
  df$min[1:dim(Min[[k]])[1]] <- Min[[k]][[2]][,1]
  df$amplitude <- df$max-df$min
  
  for(l in 0:lagmax){
    
    df <- cbind(df, data.frame(sst=NA, t2m=NA))
    
    df$sst <- climate$sst[match(df$date, climate$date)-l]
    df$t2m <- climate$t2m[match(df$date, climate$date)-l]
    
    colnames(df)[which(colnames(df)=="sst")] <- paste0("sst_lag", l)
    colnames(df)[which(colnames(df)=="t2m")] <- paste0("t2m_lag", l)
    
  }
  
  df <- na.omit(df)
  df <- df[2:(dim(df)[1]-1),]
  
  df$metabolheat <- (1:dim(df)[1])/dim(df)[1]
  
  data <- rbind(data, df)
  
}

dfArima <- cbind(expand.grid(p=0:1, q=0:1), AIC=0)

for(i in 1:nrow(dfArima)){
  
  for(j in 1:length(unique(data$logger))){
    
    df <- data[data$logger==unique(data$logger)[j],]
    
    dfArima[i, "AIC"] <- dfArima[i, "AIC"]+arima(df$mean[-c(1, nrow(df))], 
                                                 order = c(dfArima[i, "p"], 0, dfArima[i, "q"]), 
                                                 optim.control = list(maxit=10000), 
                                                 method = "ML", include.mean = TRUE)$aic
    
  }
  
}

plot(dfArima$AIC, type="l")
arrows(which.min(dfArima$AIC), max(dfArima$AIC), which.min(dfArima$AIC), min(dfArima$AIC), col="red")

p <- dfArima[which.min(dfArima$AIC), "p"]
q <- dfArima[which.min(dfArima$AIC), "q"]

cs1ARMA <- corARMA(form = ~ 1 | logger, p=p, q=q)
cs1ARMA. <- Initialize(cs1ARMA, data = data)

lags <- cbind(expand.grid(t2m=0:lagmax, sst=0:lagmax), data.frame(AIC=NA))

best <- NULL
aic <- +Inf

pb <- txtProgressBar(min = 1, max = dim(lags)[1], initial = 1) 

for(k in seq_along(lags$t2m)){
  
  formula <- as.formula(paste0("mean ~ ", "sst_lag", lags$sst[k], " + t2m_lag", lags$t2m[k], " + metabolheat"))
  
  GLMM <- lme(formula, 
              random= ~ 1 | logger, 
              correlation= cs1ARMA., 
              data = data, 
              method="ML", 
              control=list(maxIter = 5000, msMaxIter = 5000))
  
  lags$AIC[k] <- AIC(GLMM)
  
  if(AIC(GLMM)<aic){
    best <- GLMM
    aic <- AIC(GLMM)
  }
  
  setTxtProgressBar(pb,k)
  
}

close(pb)

plot(lags$AIC, type="l")
arrows(which.min(lags$AIC), max(lags$AIC), which.min(lags$AIC), min(lags$AIC), col="red")

GLMM <- best
sst_lag <- lags$sst[which.min(lags$AIC)]
t2m_lag <- lags$t2m[which.min(lags$AIC)]

formula <- as.formula(paste0("mean ~ ", "sst_lag", lags$sst[which.min(lags$AIC)], " + t2m_lag", lags$t2m[which.min(lags$AIC)], " + metabolheat"))

GLM <- glm(formula, data = data)

save(climate, data, GLMM, GLM, lags, timezone, meantimemax, meantimemin, file=file.path("dataOut", "GLMM_lags.RData"))
##


##
load(file.path("dataOut", "GLMM_lags.RData"))

hist(data$amplitude)
amplitude <- mean(data$amplitude)

GLMM_summary <- summary(GLMM)
GLM_summary <- summary(GLM)

sst_lag <- lags$sst[which.min(lags$AIC)]
t2m_lag <- lags$t2m[which.min(lags$AIC)]

intercept <- GLM_summary$coefficients["(Intercept)","Estimate"]
coef_sst <- GLM_summary$coefficients[paste0("sst_lag", sst_lag),"Estimate"]
coef_t2m <- GLM_summary$coefficients[paste0("t2m_lag", t2m_lag),"Estimate"]
coef_metabolheat <- GLM_summary$coefficients["metabolheat","Estimate"] 

random_coef <- c(GLMM_summary$coefficients$random$logger)
hist(random_coef, col="grey", main = "Random effect")
sd_rdm <- sd(random_coef)

date <- seq(as.POSIXlt("1978-01-01", tz="UTC"), as.POSIXlt("2023-01-01", tz="UTC"), by="1 day")

date_max <- (as.numeric(date+meantimemax*60*60)-as.numeric(date[1]))/60 # in minutes
date_min <- (as.numeric(date+meantimemin*60*60)-as.numeric(date[1]))/60 # in minutes

incubtemp <- data.frame(datetime=c(as.character(date[1]+date_min*60), as.character(date[1]+date_max*60)), 
                        time=c(date_min, date_max))

incubtemp <- incubtemp[order(incubtemp$time),]

infosites <- read.csv(file.path("~", "Projects", "Adaptive Capacity of Sea Turtles", "WarmingScenarios_AR6", "infosites_warming_AR6.csv"))

folderlabel <- gsub("C:/Users/Jonathan/Documents/Projects/Adaptive Capacity of Sea Turtles/Scripts/", "", getwd())

scenario <- c("present", "rcp4.5", "rcp4.5_P25", "rcp4.5_P75", "rcp8.5", "rcp8.5_P25", "rcp8.5_P75")

warming_air <- c(0, 
                 as.numeric(infosites[which(infosites$Label==folderlabel), "RCP4.5.Air"]), 
                 as.numeric(unlist(strsplit(infosites[which(infosites$Label==folderlabel), "RCP4.5.Air..P25_P75."], "_"))),
                 as.numeric(infosites[which(infosites$Label==folderlabel), "RCP8.5.Air"]),
                 as.numeric(unlist(strsplit(infosites[which(infosites$Label==folderlabel), "RCP8.5.Air..P25_P75."], "_"))))

warming_sea <- c(0, 
                 as.numeric(infosites[which(infosites$Label==folderlabel), "RCP4.5.Sea"]), 
                 as.numeric(unlist(strsplit(infosites[which(infosites$Label==folderlabel), "RCP4.5.Sea..P25_P75."], "_"))),
                 as.numeric(infosites[which(infosites$Label==folderlabel), "RCP8.5.Sea"]),
                 as.numeric(unlist(strsplit(infosites[which(infosites$Label==folderlabel), "RCP8.5.Sea..P25_P75."], "_"))))

maxlag <- max(c(sst_lag, t2m_lag))

firstday <- as.Date("1979-01-01")+maxlag

climate_lag <- data.frame(date=as.character(date), sst=NA, t2m=NA)

ind.firstday <- which(substr(climate_lag$date, 1, 10)==as.character(firstday))

sst_pos <- ifelse(sst_lag==maxlag, 1, maxlag-sst_lag+1)
t2m_pos <- ifelse(t2m_lag==maxlag, 1, maxlag-t2m_lag+1)

sst_kept <- climate$sst[sst_pos:length(climate$sst)]
t2m_kept <- climate$t2m[t2m_pos:length(climate$t2m)]

rowstofill <- ind.firstday:dim(climate_lag)[1]

climate_lag$sst[rowstofill] <- c(sst_kept, rep(NA, length(rowstofill)-length(sst_kept)))
climate_lag$t2m[rowstofill] <- c(t2m_kept, rep(NA, length(rowstofill)-length(t2m_kept)))

for(k in seq_along(scenario)){
  
  temperature <- intercept+coef_sst*(climate_lag$sst+warming_sea[k])+coef_t2m*(climate_lag$t2m+warming_air[k])
  
  df <- data.frame(temperature=c(temperature-(amplitude/2), temperature+(amplitude/2)),
                   time=c(date_min, date_max))
  
  df <- df[order(df$time),]
  
  incubtemp <- cbind(incubtemp, data.frame(temperature=df$temperature))
  colnames(incubtemp)[which(colnames(incubtemp)=="temperature")] <- scenario[k]
  
}

incubtemp <- na.omit(incubtemp)

attr(incubtemp, "amplitude") <- amplitude
attr(incubtemp, "timemin") <- meantimemin
attr(incubtemp, "timemax") <- meantimemax

attr(incubtemp, "lag_sst") <- sst_lag
attr(incubtemp, "lag_t2m") <- t2m_lag
attr(incubtemp, "intercept") <- intercept
attr(incubtemp, "coef_sst") <- coef_sst
attr(incubtemp, "coef_t2m") <- coef_t2m
attr(incubtemp, "coef_metabolheat") <- coef_metabolheat
attr(incubtemp, "sd_rdm") <- sd_rdm

attr(incubtemp, "sd_low_rcp4.5") <- unique(incubtemp$rcp4.5-incubtemp$rcp4.5_P25)[1]
attr(incubtemp, "sd_up_rcp4.5") <- unique(incubtemp$rcp4.5_P75-incubtemp$rcp4.5)[1]
attr(incubtemp, "sd_low_rcp8.5") <- unique(incubtemp$rcp8.5-incubtemp$rcp8.5_P25)[1]
attr(incubtemp, "sd_up_rcp8.5") <- unique(incubtemp$rcp8.5_P75-incubtemp$rcp8.5)[1]

attr(incubtemp, "RCP4.5_air") <- warming_air[2]
attr(incubtemp, "RCP4.5_air_P25") <- warming_air[3]
attr(incubtemp, "RCP4.5_air_P75") <- warming_air[4]
attr(incubtemp, "RCP8.5_air") <- warming_air[5]
attr(incubtemp, "RCP8.5_air_P25") <- warming_air[6]
attr(incubtemp, "RCP8.5_air_P75") <- warming_air[7]

attr(incubtemp, "RCP4.5_sea") <- warming_sea[2]
attr(incubtemp, "RCP4.5_sea_P25") <- warming_sea[3]
attr(incubtemp, "RCP4.5_sea_P75") <- warming_sea[4]
attr(incubtemp, "RCP8.5_sea") <- warming_sea[5]
attr(incubtemp, "RCP8.5_sea_P25") <- warming_sea[6]
attr(incubtemp, "RCP8.5_sea_P75") <- warming_sea[7]

attr(incubtemp, "timezone") <- timezone

save(incubtemp, file=file.path("dataOut", "incubtemp_lags.RData"))
