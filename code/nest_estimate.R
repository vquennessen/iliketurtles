################################################################################
# linear model based on previous year's total number of nests
setwd('C:/Users/vique/Dropbox/NSF_IOS_2019_2020_FDN/Database/Database_season 1')

# load libraries
library(readxl)

# load nests file
nests <- read_xlsx(path = 'Ninhos_FEN.xlsx', 
                    range = 'A1:C39')

# basic linear model
model1 <- lm(ninhos ~ X, data = nests)
summary(model1)

# prediction interval for next year (2021/2022)
new_year <- data.frame(X = 39)
predict(model1, new_year, interval = 'predict')
# fit        lwr      upr
# 1 219.0213 66.19585 371.8468

################################################################################
# method 2
setwd('C:/Users/vique/Dropbox/NSF_IOS_2019_2020_FDN/Database/Database_season 1')

# load libraries
library(readxl)
library(dplyr)
library(ggplot2)

# read in datafile
nests <- read_xlsx(path = 'exportacao_ocorrencia_reprodutiva.xlsx', 
                   sheet = 'exportacao_ocorrencia_reproduti')

# calculate number of nests per month per season
nests_by_month <- nests %>%
  group_by(campanha, mes) %>%
  count()

# plot nest counts by month by season
ggplot(data = nests_by_month, 
       mapping = aes(x = mes, y = n, col = campanha)) +
  geom_line() +
  geom_hline(yintercept = 24) +
  scale_x_continuous(breaks = 1:12)

# nests by end of January for each season
nests_by_year <- nests %>%
  filter(mes == 1 | mes > 8) %>%
  group_by(campanha) %>%
  count() %>%
  ungroup() %>%
  arrange(n, decreasing = FALSE)

# filter plot by years with similar totals by end of January
seasons <- c('15/16', '11/12', '17/18', '07/08')

similar_years <- nests %>%
  filter(campanha %in% seasons) %>%
  group_by(campanha, mes) %>%
  count()

# total number of nests by these seasons
nests %>%
  filter(campanha %in% seasons) %>%
  group_by(campanha) %>%
  count()

