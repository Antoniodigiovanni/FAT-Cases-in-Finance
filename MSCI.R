library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(rlang)
library(readxl)
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
annCountryWeights <- annMW
#calculate the annual Return per country * weight
annMW <- annMW[-c(1),]
annMW<-subset(annMW, annMW$year >= 1998)

MSCI_weighted <- annMW %>% mutate( TWN = TWN * MSCI_Country_Returns$TAI, HKG = HKG * MSCI_Country_Returns$HKG, KOR = KOR * MSCI_Country_Returns$KOR, SGP = SGP*MSCI_Country_Returns$SGP)
MSCI_weighted <- MSCI_weighted %>% mutate(ret=TWN+KOR+HKG+SGP) %>%
  select(year,ret)
MSCI_weighted$annRet<-MSCI_weighted$ret-1


#Annualized Return
AR<-as.data.frame(mean(MSCI_weighted$annRet)*100)
colnames(AR)[1]<-"Annualized Return"


names(annCountryWeights)[2]<-"HKG_w"
names(annCountryWeights)[3]<-"KOR_w"
names(annCountryWeights)[4]<-"SGP_w"
names(annCountryWeights)[5]<-"TWN_w"
  
MSCI_monthly <- read_excel("MSCI_monthly.xlsx")
MSCI_monthly$year <- year(MSCI_monthly$Date)

MSCI_monthly<-merge(MSCI_monthly,annCountryWeights, by = "year")
MSCI_monthly <- MSCI_monthly %>% mutate(HKG = HKG * HKG_w, TAI = TAI * TWN_w, SGP = SGP*SGP_w, KOR= KOR*KOR_w)%>%
  mutate(MSCI_ret = TAI+HKG+SGP+KOR-1)%>%
  select(Date, MSCI_ret, year)

MSCI_monthly_std <- MSCI_monthly %>% group_by(year)%>%summarise(std=sd(MSCI_ret)*sqrt(12))

#Volatility <- sd(MSCI_monthly$MSCI_ret)
#How we approached at the portfolios:
Volatility <- as.data.frame(mean(MSCI_monthly_std$std))
colnames(Volatility)[1]<-"Volatility"

Results_MSCI <- merge(AR, Volatility)
rownames(Results_MSCI)[1]<-"MSCI"

MSCI_weighted <- MSCI_weighted %>% mutate(Portfolio_Value = 100*lag(cumprod(annRet+1)))
MSCI_weighted$Portfolio_Value[1]=100
rm(AR, Volatility)



MSCI_Country_Returns_m  <- read_excel("MSCI_monthly.xlsx")
MSCI_Country_Returns_m <- MSCI_Country_Returns_m %>% mutate(year=year(Date))
MSCI_Country_Returns_m<-subset(MSCI_Country_Returns_m, MSCI_Country_Returns_m$year >= 1998)
MSCI_Country_Returns_m <- merge(MSCI_Country_Returns_m, annCountryWeights, by=c("year"))
MSCI_Country_Returns_m <- MSCI_Country_Returns_m %>% mutate(MarketReturn = HKG * HKG_w+SGP*SGP_w+KOR*KOR_w+TAI*TWN_w)
write.csv(MSCI_Country_Returns_m, "MSCI_weighted_monthly")

