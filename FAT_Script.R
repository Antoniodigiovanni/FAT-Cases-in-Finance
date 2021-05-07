###FAT
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

getwd()

setwd("~/Documents/04_TUM/Cases is Finance/FAT")

load("~/Documents/04_TUM/Cases is Finance/FAT/FAT_yearly.RData")
load("~/Documents/04_TUM/Cases is Finance/FAT/FAT_static.RData")
load("~/Documents/04_TUM/Cases is Finance/FAT/FAT_monthly.RData")

###RMRF

minYear <- min(FAT.yearly$YEAR)
maxYear <- max(FAT.yearly$YEAR)
allEntries<- nrow(FAT.yearly)

##Market Weights
totalMarketCap <- data.frame("YEAR"=minYear:maxYear, "TOTALMC"=0)

for ( i in 1 : (maxYear-minYear+1)){
  meta <- FAT.yearly[YEAR==(minYear-1+i)]
  totalMarketCap[i,2] <- sum(meta$WC08001, na.rm=TRUE)
}

totalMarketCap$YEAR <-as.numeric(totalMarketCap$YEAR)

FAT.monthly$YEAR <- as.numeric(format(FAT.monthly$Date,'%Y'))

FAT_Work <- FAT.monthly

FAT_Work <- merge(FAT_Work
                  , FAT.yearly, by=c("Id","YEAR"))
FAT_Work <- merge(FAT_Work
                  , totalMarketCap, by="YEAR")
#sum(FAT_Work$TOTALMC, na.rm =TRUE)

##Weighted Return

FAT_Work$MarketWeight <- (FAT_Work$WC08001/FAT_Work$TOTALMC)
FAT_Work$weightedReturn <- FAT_Work$MarketWeight * FAT_Work$RET.USD

#test
#sum(FAT_Work$MarketWeight, na.rm=TRUE)
#sum(FAT_Work$weightedReturn, na.rm=TRUE)

##RM
annualMarketReturn <- data.frame("YEAR"=minYear:maxYear, "RM"=0)

for ( i in minYear : maxYear){
  meta <- FAT_Work[YEAR==(i)]
  annualMarketReturn$RM[(i-minYear+1)] <- sum(meta$weightedReturn, na.rm=TRUE)
}

FAT_Work <- merge(annualMarketReturn, FAT_Work, by="YEAR")

sum(FAT_Work$RM, na.rm=TRUE)


##RMRF
#merge with 30 years treasury yield from: https://fred.stlouisfed.org/series/DGS30
#RF
library(readxl)
DGS30_2 <- read_excel("DGS30-2.xls")
Riskfree <- data.frame(DGS30_2)
names(Riskfree)[names(Riskfree) == "observation_date"] <- "Date"
names(Riskfree)[names(Riskfree) == "DGS30"] <- "RF"
#Riskfree$observation_date <- as.numeric(Riskfree$observation_date)
Riskfree$Date <- as.numeric(format(FAT.monthly$Date,'%Y-%M-%D', tz = NULL))

#align Date format
strip.tz <- function(dt) {
  fmt <- "%Y-%m-%d"
  strptime(strftime(dt, format = fmt, tz="UTC"), format = fmt, tz="UTC")
}
FAT_Work$Date <- strip.tz(FAT_Work$Date)
Riskfree$Date <- strip.tz(Riskfree$Date)
#Riskfree$Date
#FAT_Work$Date

#FAT_Work.RM1 <- FAT_Work
FAT_Work <- merge(FAT_Work, Riskfree, by= "Date")
#RMRF
sum(FAT_Work$RM, na.rm=TRUE)
FAT_Work$RMRF=FAT_Work$RM-FAT_Work$RF

sum(FAT_Work$RM, na.rm=TRUE)
sum(FAT_Work$RMRF, na.rm=TRUE)
saveRDS(FAT_Work, file = "FAT_Work.RData")

#Add Beta to FAT.Static
for (j in 1:nrow(FAT.static)){
  ID <- FAT.static$Id[j]
  if (all(is.na(FAT_Work$RET.USD[which(FAT_Work$Id == ID)]))==F && all(is.na(FAT_Work$RMRF[which(FAT_Work$Id == ID)]))==F){
    if(length(summary(lm(FAT_Work$RET.USD[which(FAT_Work$Id == ID)] ~ FAT_Work$RMRF[which(FAT_Work$Id == ID)]))$coefficients[,1]) > 1){
    print(j)
    FAT.static$BETA[j] <- summary(lm(FAT_Work$RET.USD[which(FAT_Work$Id == ID)] ~ FAT_Work$RMRF[which(FAT_Work$Id == ID)]))$coefficients[2,1]    
    }else{FAT.static$BETA[j]<-NA}
  }else{
    FAT.static$BETA[j]<-NA
  }
}
#saveRDS(FAT.static, file = "FAT.static")


FAT_Work$BtoM <- (FAT_Work$WC03501/(FAT_Work$CNOSH*FAT_Work$PCH))
