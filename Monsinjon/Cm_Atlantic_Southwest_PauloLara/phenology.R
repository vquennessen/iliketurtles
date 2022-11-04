
library(phenology)

# each row refers to a number of nests
# a missing day means (see monitoring effort below):
#2015/2016: 2015-12-01 to 2016-05-15
#2016/2017: 2016-12-01 to 2017-07-24
#2017/2018: 2017-12-01 to 2018-07-30
#2018/2019: 2019-01-01 to 2019-04-30
#2019/2020: 2019-12-17 to 2020-03-19 (-2019-12-31 not monitored)

#### load nest counts ####

data <- read.csv(file.path("dataIn", "NestCount.csv"), sep=";")

data <- data.frame(date=data$Date.Nesting, 
                   number=data$Number.of..nests, 
                   stringsAsFactors=FALSE)

data$date <- as.character(as.Date(data$date, format="%m/%d/%Y"))

year.start <- min(as.numeric(substr(data$date, 1, 4)), na.rm=TRUE)
year.end <- max(as.numeric(substr(data$date, 1, 4)), na.rm=TRUE)

timeframe <- seq(as.Date(paste0(year.start, "-01-01")), as.Date(paste0(year.end, "-12-31")), by="1 day") 

daily.sum <- aggregate(number ~ cut(as.Date(data$date), breaks=timeframe), data = data, FUN="sum")

# prepare an empty data frame
NbNests <- data.frame(date=as.character(timeframe), 
                      number=NA, 
                      stringsAsFactors=FALSE)

NbNests$number[which(as.Date(NbNests$date)>=as.Date("2015-12-01") & as.Date(NbNests$date)<=as.Date("2016-05-15"))] <- 0
NbNests$number[which(as.Date(NbNests$date)>=as.Date("2016-12-01") & as.Date(NbNests$date)<=as.Date("2017-07-24"))] <- 0
NbNests$number[which(as.Date(NbNests$date)>=as.Date("2017-12-01") & as.Date(NbNests$date)<=as.Date("2018-07-30"))] <- 0
NbNests$number[which(as.Date(NbNests$date)>=as.Date("2019-01-01") & as.Date(NbNests$date)<=as.Date("2019-04-30"))] <- 0
NbNests$number[which(as.Date(NbNests$date)>=as.Date("2019-12-17") & as.Date(NbNests$date)<=as.Date("2020-03-19"))] <- 0
NbNests$number[which(NbNests$date=="2019-12-31")] <- NA

# replace NAs and zeros by number of nests
NbNests$number[match(as.character(daily.sum[,1]), NbNests$date)] <- daily.sum$number

# check if everything looks fine
plot(as.Date(NbNests$date), NbNests$number)

save(NbNests, file=file.path("dataOut", "NestCounts.RData"))

