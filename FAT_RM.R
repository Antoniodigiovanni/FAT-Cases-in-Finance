library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

load(file.path("Data","FAT_monthly.RData"))
load(file.path("Data","FAT_static.RData"))
load(file.path("Data","FAT_yearly.RData"))


#Calculating Total Market Cap for each month
Monthly_Market_Cap <- data.frame(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "MV.USD" = FAT.monthly$MV.USD)

Monthly_Market_Cap <- Monthly_Market_Cap %>%
  drop_na(MV.USD) %>% 
  group_by(ym) %>% 
  summarise(MV.TOT.USD = sum(MV.USD)) %>% 
  arrange(ym, .by_group=TRUE)


# Calculate Weights + sum the product of single Return * Weights
Market_Portfolio_FAT <- data.frame(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "RET.USD" = FAT.monthly$RET.USD,
  "MV.USD" = FAT.monthly$MV.USD)

Market_Portfolio_FAT <- left_join(Market_Portfolio_FAT, 
                              Monthly_Market_Cap, 
                              by = (c("ym" = "ym")))

rm(Monthly_Market_Cap)

Market_Portfolio_FAT <- Market_Portfolio_FAT %>% 
  drop_na(MV.USD, RET.USD) %>% 
  mutate(Weight = (MV.USD/MV.TOT.USD)) %>% 
  group_by(ym) %>% 
  summarise(RM = sum(RET.USD*Weight))


# The 1-month TBill return is from Ibbotson and Associates, Inc. 
# (Kennet French's Website)

FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"))

Market_Portfolio_FAT <- left_join(Market_Portfolio_FAT,
                              FF,
                              by = "ym")
Market_Portfolio_FAT <- Market_Portfolio_FAT %>% 
  select(-`Mkt-RF`,-SMB, -HML) %>% 
  mutate(RMRF = (RM - RF))


rm(FF,FAT.monthly,FAT.static,FAT.yearly)
