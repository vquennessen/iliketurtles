
library(embryogrowth)

path <- file.path("R", "Scripts", "Cm_Atlantic_Southwest_PauloLara")

#### fit the growth rate reaction norm ####

load(file.path(path, "dataOut", "NestTemperatures.RData"))

# 1. format nest temperatures using FormatNests()

## first nest
nest1 <- data.frame(Time=NestTemps[[1]][,"time"], Temperature=NestTemps[[1]][,"temperature"])
colnames(nest1)[2] <- names(NestTemps)[1] # makes sure each nest has a unique name

formatted <- FormatNests(nest1, previous=NULL)

## other nests
for(k in 2:length(NestTemps)){
  
  nestX <- data.frame(Time=NestTemps[[k]][,"time"], Temperature=NestTemps[[k]][,"temperature"])
  colnames(nestX)[2] <- names(NestTemps)[k] # makes sure each nest has a unique name
  
  formatted <- FormatNests(nestX, previous=formatted)
  
}

# 2. now we can use the formatted data to fit the Growth Rate Thermal Reaction Norm (GRTRN) using searchR()

## parameter initialization
SCLdata <- read.csv(file.path(path, "dataIn", "HatchlingMeasurements.csv"))
SCLmean <- mean(SCLdata$SCL..cm., na.rm=T)*10 # Mean = 49.14051 mm
SCLsd <- sd(SCLdata$SCL..cm., na.rm=T)*10 # SD = 3.156266 mm

testSCL <- c(Mean=SCLmean, SD=SCLsd)
parinit <- c("DHA"=170, "DHH"=200, "T12H"=300, "Rho25"=385) # parameters to be fitted (these initial values seem to work in most cases)
pfixed <- c(rK=1.208968) # fixed parameters for the Gompertz growth function (do not change)
X0 = 0.3470893 # initial embryo size (do not change)

## fit the curve using Maximum Likelihood
GRTRN <- searchR(parameters=parinit, 
                 temperatures=formatted, 
                 hatchling.metric=testSCL)

save(GRTRN, file=file.path(path, "dataOut", "GRTRN.RData"))

load(file.path(path, "dataOut", "GRTRN.RData"))

plotR(GRTRN)


## estimate confidence intervals with Bayesian MCMC
pMCMC <- TRN_MHmcmc_p(GRTRN, accept=TRUE)

GRTRN_mcmc <- GRTRN_MHmcmc(result=GRTRN,
                           parametersMCMC=pMCMC, 
                           n.iter=10000, 
                           adaptive=TRUE, 
                           trace=TRUE)

save(GRTRN, GRTRN_mcmc, file=file.path(path, "dataOut", "GRTRN.RData"))

plotR(GRTRN, resultmcmc=GRTRN_mcmc, curve="MCMC quantiles", show.hist=TRUE, ylimH=c(0, 0.5))

