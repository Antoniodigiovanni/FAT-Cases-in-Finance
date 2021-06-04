library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(rlang)
source("Real_data_prep.R")
MSCI_Country_Returns <- read_excel("MSCI Country Returns.xls")

stocks <- all_data %>% select(Id, country, ym, year,  MV.USD.June )
#Calc annual country weights
annMW <- stocks %>% group_by(year, country) %>% summarise(countryMV=sum(MV.USD.June))
totMV <- annMW %>% group_by(year) %>% summarise (totalMV =sum(countryMV))
annMW <- merge(annMW, totMV, by = "year")
annMW <- annMW %>% mutate(countryWeight = countryMV/totalMV)
annMW <- annMW%>% select(year, country, countryWeight)
annMW <- annMW %>% spread(country, countryWeight)

#calculate the annual Return per country * weight
annMW <- annMW[-c(1),]
MSCI_weighted <- annMW %>% mutate( TWN = TWN * MSCI_Country_Returns$TAI, HKG = HKG * MSCI_Country_Returns$HKG, KOR = KOR * MSCI_Country_Returns$KOR, SGP = SGP*MSCI_Country_Returns$SGP)
MSCI_weighted <- MSCI_weighted %>% mutate(ret=TWN+KOR+HKG+SGP) %>%
  select(year,ret)
