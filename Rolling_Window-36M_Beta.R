library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

load("FAT_monthly.RData")


#### RMRF Calculation ####
#Calculating Total Market Cap for each month
Monthly_Market_Cap <- data.frame(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "MV.USD" = FAT.monthly$MV.USD)

Monthly_Market_Cap <- Monthly_Market_Cap %>%
  drop_na(MV.USD) %>% 
  group_by(Country, ym) %>% 
  summarise(MV.TOT.USD = sum(MV.USD)) %>% 
  arrange(ym, .by_group=TRUE)


# Calculate Weights + sum the product of single Return * Weights
Market_Portfolio <- data.frame(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "RET.USD" = FAT.monthly$RET.USD,
  "MV.USD" = FAT.monthly$MV.USD)

Market_Portfolio <- left_join(Market_Portfolio, 
                              Monthly_Market_Cap, 
                              by = (c("Country" = "Country","ym" = "ym")))

rm(Monthly_Market_Cap)

Market_Portfolio <- Market_Portfolio %>% 
  drop_na(MV.USD, RET.USD) %>% 
  mutate(Weight = (MV.USD/MV.TOT.USD)) %>% 
  group_by(Country, ym) %>% 
  summarise(RM = sum(RET.USD*Weight))


# The 1-month TBill return is from Ibbotson and Associates, Inc. 
# (Kennet French's Website)

FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"))

Market_Portfolio <- left_join(Market_Portfolio,
                              FF,
                              by = "ym")
Market_Portfolio <- Market_Portfolio %>% 
  select(-`Mkt-RF`,-SMB, -HML) %>% 
  mutate(RMRF = (RM - RF))

rm(FF)

#### Beta Calculation ####

## Rollapply Function (NOT WORKING)
# beta.regr <- function(x) {
#   if (sum(complete.cases(x)) >= 12) coef(lm(RET.USD ~ RMRF, data = as.data.frame(x))) else c(NA, NA)
# }
# 
# 
# test <- rollapply(
#   data = dataset,
#   36,
#   by = 12,
#   beta.regr,
#   fill=NA,
#   by.column = FALSE
# )


#### Test with multiple IDs in a for loop #####

Beta_Regr <- data.table(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "RET.USD" = FAT.monthly$RET.USD)

Beta_Regr <- left_join(
  Beta_Regr,
  Market_Portfolio[c("Country","ym","RMRF")],
  by = c("Country","ym"))

Ids <- as.character(unique(Beta_Regr$Id))
#Ids <- Ids[1:100] # Shorten Id list to check if the loop works
BetaList = list()
DatesList = list()
IdList = list()
k=0
for (id in Ids) {
  i=0
  df <- Beta_Regr[Id == id]
  months <- df$ym
  for (date in months) {
    date <- as.yearmon(date)
    if (month(date) == 6) { #Update only in June
      x <- df[ym<=date & ym > date-(36/12)]
      if (sum(complete.cases(x$RET.USD))>=12 & !is.na(df[ym==date]$RET.USD)){ #Should I check only for RET.USD?
        i = i+1
        k = k+1
        Beta <- as.double(coef(lm(RET.USD~RMRF, data = x))[2])
        BetaList[[k]] <- Beta
        DatesList[[k]] <- date
        IdList[[k]] <- as.character(x$Id[i])
        }
    }
  }
}


Beta_36_M <- as.data.frame(cbind(IdList, DatesList, BetaList))
Beta_36_M <- Beta_36_M %>% 
  rename(
    "Id" = "IdList",
    "ym" = "DatesList",
    "Beta" = "BetaList"
  ) %>% 
  mutate(ym = as.yearmon(ym))

rm(Beta_Regr,BetaList,DatesList,
   df,IdList,Market_Portfolio,x,
   Beta,date,i,id,Ids,months,k)

save(Beta_36_M, file = "Beta_36_M.RData")
# Missing the Beta Merge into the main df...

