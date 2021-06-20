library(rstudioapi)
library(tidyverse)
library(zoo)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 
# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

FF5M <- read_csv("global_factor_premia_data.csv") %>% 
  filter(market == "SGP" | 
         market == "HKG" |
         market == "TWN" |
         market == "KOR") %>% 
  mutate(ym = as.yearmon(Date)) %>% 
  filter(ym >= "Jul 1998") %>% 
  filter(variable == "RMRF" |
           variable == "SMB" |
           variable == "HML" |
           variable == "CMA" |
           variable == "MOM" |
           variable == "RMW_OPtBE") %>%
  rename(Country = market) %>% 
  mutate(return = return/100) #,cum = 100* (1 + ret))  

## Calculating Market Cap of each country - in order to value-weight the FF5Fs
load(file.path("Data", "FAT_monthly.RData"))

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
  summarise(Country_MV = sum(MV.USD)) %>% 
  arrange(ym, .by_group=TRUE)

Monthly_Total_Region_Market_Cap <- Monthly_Market_Cap %>% 
  group_by(ym) %>% 
  summarise(Region_MV = sum(Country_MV))

FF5M <- left_join(FF5M, Monthly_Market_Cap, by=c("Country", "ym"))
FF5M <- left_join(FF5M, Monthly_Total_Region_Market_Cap, by=c("ym"))

rm(Monthly_Market_Cap, Monthly_Total_Region_Market_Cap, FAT.monthly)

FF5M <- FF5M %>% mutate(Weight = Country_MV/Region_MV) %>% 
  group_by(variable, ym) %>% 
  summarise(RET = sum(return*Weight))

FF5M <- FF5M %>% pivot_wider(names_from = variable, values_from = RET)

# Change this to load only the returns RData file
source("factor_portfolio.R")

EW_Factor_Portfolio_Monthly_RET <- eq_cumreturn

rm(list = setdiff(ls(), c("FF5M", "EW_Factor_Portfolio_Monthly_RET")))  # Remove all but what is needed

FF5M <- left_join(FF5M, EW_Factor_Portfolio_Monthly_RET, by=c("ym")) %>% 
  rename(Portfolio_RET = monthly_ret) %>% 
  mutate(Portfolio_RET = Portfolio_RET/100) 


FF5M_Regression <- summary(lm(Portfolio_RET ~ RMRF + HML + SMB + CMA + RMW_OPtBE + MOM, data = FF5M))
FF5M_Regression

