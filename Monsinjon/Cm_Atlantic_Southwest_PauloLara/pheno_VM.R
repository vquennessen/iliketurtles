
library(phenology)

path <- file.path("R", "Scripts", "Cm_Atlantic_Southwest_PauloLara")

#### fit the dynamics of nesting seasons ####

load(file.path(path, "dataOut", "NestCounts.RData"))

# 1. define nesting seasons

NbNests <- cbind(NbNests, data.frame(season=NA))

reference.month <- "09" # cut-off month to define the borders of each nesting season (should be around 06 if the peak seems to be during Dec/Jan)

year.start <- as.numeric(substr(NbNests$date[1], 1, 4))
year.end <- as.numeric(substr(tail(NbNests$date, n=1), 1, 4))

cutoff.dates1 <- seq(as.Date(paste0(year.start, "-", reference.month, "-01")), as.Date(paste0(year.end, "-", reference.month, "-01")), by="1 year")
cutoff.dates2 <- seq(as.Date(paste0(year.start+1, "-", reference.month, "-01"))-1, as.Date(paste0(year.end+1, "-", reference.month, "-01"))-1, by="1 year")

for(k in seq_along(cutoff.dates1)){
  
  season.label <- c(substr(cutoff.dates1[k], 1, 4), substr(cutoff.dates2[k], 1, 4))
  season.label <- ifelse(length(unique(season.label))==1, unique(season.label), paste0(season.label[1], "-", season.label[2]))
  
  NbNests$season[which(as.Date(NbNests$date)>=cutoff.dates1[k] & as.Date(NbNests$date)<=cutoff.dates2[k])] <- season.label
  
}

# 2. format nest counts using add_phenology()

NbNests <- na.omit(NbNests) # if any, remove NAs (dates we don't know the number of nests for)

formatted <- add_phenology(add=NbNests, colname.Date="date", colname.Number="number", previous=NULL, month_ref=as.numeric(reference.month), format="%Y-%m-%d", sep.dates = "_", silent=TRUE)

# 3. now we can use the formatted data to fit the overall dynamics of nesting seasons (i.e., the curve that describes the number of nests per day) using fit_phenology()

## parameter initialization
pfixed <- c(Flat=0, Min=0)
pari <- par_init(data=formatted, fixed.parameters=pfixed)

## fit the curve using Maximum Likelihood (WARNING: This might take a long time!!! So make sure you save the results.)
phenoglobal <- fit_phenology(data=formatted, fitted.parameters=pari, fixed.parameters=pfixed)

## save results
save(phenoglobal, formatted, file=file.path(path, "dataOut", "phenoglobal.RData"))

# 4. now we want to fit a curve specifically for each nesting season (roughly same procedure but allows season-specific parameters to vary)

## first, we keep the "scale" parameters (Max) from previous fit
parMax <- phenoglobal$par[which(substr(names(phenoglobal$par), 1, 3)=="Max")]

pfixed <- c(parMax, Flat=0, Min=0)

## and we fit the "shape" parameters (Length, Peak, Theta) specifically for each season
pari <- phenoglobal$par[which(substr(names(phenoglobal$par), 1, 3)!="Max")]

seasons <- unlist(lapply(strsplit(names(parMax), "_"), FUN=function(x) x[2]))

parLB <- rep(pari["LengthB"], length(seasons))
names(parLB) <- paste0("LengthB_", seasons)

parLE <- rep(pari["LengthE"], length(seasons))
names(parLE) <- paste0("LengthE_", seasons)

parPeak <- rep(pari["Peak"], length(seasons))
names(parPeak) <- paste0("Peak_", seasons)

pari <- c(pari[!names(pari) %in% c("LengthB", "LengthE", "Peak")], parLB, parLE, parPeak)

phenospecific1 <- fit_phenology(data=formatted, fitted.parameters=pari, fixed.parameters=pfixed)

## retrieve the fitted and fixed parameters
par1 <- phenospecific1$par
pfixed1 <- phenospecific1$fixed.parameters

## second, we keep the "shape" parameters we just fitted
pfixed <- c(par1, Min=0, Flat=0)

## and refine the fit for "scale" parameters 
pari <- pfixed1[!names(pfixed1) %in% c("Min", "Flat")]

phenospecific2 <- fit_phenology(data=formatted, fitted.parameters=pari, fixed.parameters=pfixed)

## retrieve the fitted and fixed parameters
par2 <- phenospecific2$par
pfixed2 <- phenospecific2$fixed.parameters

## and lastly we fit both "scale" and "shape" parameters
pari <- c(par2, pfixed2[!names(pfixed2) %in% c("Min", "Flat")])
pfixed <- c(Min=0, Flat=0)

phenospecific3 <- fit_phenology(data=formatted, fitted.parameters=pari, fixed.parameters=pfixed)

## save results
phenospecific <- phenospecific3

save(phenospecific, formatted, file=file.path(path, "dataOut", "phenospecific.RData"))



## estimate confidence intervals with Bayesian MCMC
load(file.path(path, "dataOut", "phenoglobal.RData"))

pMCMC <- phenology_MHmcmc_p(result=phenoglobal, default.density="dnorm", accept=TRUE)

phenoglobal_mcmc <- phenology_MHmcmc(result=phenoglobal, 
                                     parametersMCMC=pMCMC,
                                     n.iter=10000, 
                                     adaptive=TRUE, 
                                     trace=TRUE)

save(phenoglobal, phenoglobal_mcmc, formatted, file=file.path(path, "dataOut", "phenoglobal.RData"))

load(file.path(path, "dataOut", "phenospecific.RData"))

pMCMC <- phenology_MHmcmc_p(result=phenospecific, default.density="dnorm", accept=TRUE)

phenospecific_mcmc <- phenology_MHmcmc(result=phenospecific, 
                                       parametersMCMC=pMCMC,
                                       n.iter=10000, 
                                       adaptive=TRUE, 
                                       trace=TRUE)

save(phenospecific, phenospecific_mcmc, formatted, file=file.path(path, "dataOut", "phenospecific.RData"))


load(file.path(path, "dataOut", "phenoglobal.RData"))
plot(phenoglobal, resultmcmc=phenoglobal_mcmc, series=1)

load(file.path(path, "dataOut", "phenospecific.RData"))
plot(phenospecific, resultmcmc=phenospecific_mcmc, series=1)

