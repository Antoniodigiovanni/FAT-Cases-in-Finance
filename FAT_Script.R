##FAT
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)


setwd("/Users/j.grimm/Documents/04_TUM/Cases is Finance/R Studio/FAT")
load("FAT_yearly.RData")
load("FAT_static.RData")
load("FAT_monthly.RData")


Data_Excursus2c$ym <- as.yearmon(Data_Excursus2c$Date)

FAT_Work <-merge(FAT.monthly[,-c("Date")]
                 , Data_Excursus2c,by="ym")
FAT_Work$YEAR <- as.numeric(format(FAT_Work$Date,'%Y'))

FAT_Work <- merge(FAT_Work[, -c("Date","Year")]
                  , FAT.yearly, by=c("Id","YEAR"))

summary(lm(FAT_Work[,RMRF] ~ FAT_Work[,RET.USD] ))

for (i in 1:length(names_csv)):
  if (names_csv[i,2] %in% colnames(FAT_Work)({
    names(FAT_Work)[names(FAT_Work) == names_csv[i,2]] <- names_csv[i,1]
  })
  
for (j in 1:nrow(FAT.static)){
     FAT.static$BETA[j] <- FAT_Work[Id==FAT.static[j,"Id"], lm(RET.USD~RMRF)$coefficient[1,2]]
}

for (j in 1:nrow(FAT.static)){
  ID <- FAT.static$Id[j]
  print(ID)
  FAT.static$BETA[j] <- summary(lm(FAT_Work[Id == ID,"RET.USD" ] ~ FAT_Work[Id == ID, "RMRF"]))$coefficients[2,1]
}      


for (j in 1:nrow(FAT.static)){
  ID <- FAT.static$Id[j]
  if (all(is.na(FAT_Work$RET.USD[which(FAT_Work$Id == ID)]))==F && all(is.na(FAT_Work$RMRF[which(FAT_Work$Id == ID)]))==F){
    #if (ID %in% FAT_Work$Id){
    if(length(summary(lm(FAT_Work$RET.USD[which(FAT_Work$Id == ID)] ~ FAT_Work$RMRF[which(FAT_Work$Id == ID)]))$coefficients[,1]) > 1){
    print(j)
    FAT.static$BETA[j] <- summary(lm(FAT_Work$RET.USD[which(FAT_Work$Id == ID)] ~ FAT_Work$RMRF[which(FAT_Work$Id == ID)]))$coefficients[2,1]    
    }else{FAT.static$BETA[j]<-NA}
  }else{
    FAT.static$BETA[j]<-NA
  }
}


CS.reg.estimtates <- FAT_Work[, .(gamma_zero=lm(RET.USD~RMRF)$coefficient[1],
                                        BETA=lm(RET.USD~RMRF)$coefficient[2]
                                        ),by=Id]

FAT_Work$RET.USD[which(FAT_Work$Id == "294082")]
