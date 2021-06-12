library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(rlang)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

#Data from Datastream
load(file.path("Data","FAT_monthly.RData"))
load(file.path("Data","FAT_static.RData"))
# Yearly Accounting Data from Worldscope
load(file.path("Data","FAT_yearly.RData"))

# Get data from year 1994 to ensure data quality
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly <- FAT.monthly %>% filter(year > 1993)

small_static <- FAT.static %>% select(Id, INDM)

all_data <- merge(FAT.monthly, small_static, by = "Id")
#table(small_static$INDM)

# Banks, Consumer Finance, FInancial Admin., Insurance Brokers, Mortgage Finance, Investment Services,
# Specialty Finance, Venture Capital Trust, Private Equity, Real Estate Hold, Dev, Reinsurance,
# Life Insurance, Asset Managers
# Exclude financial companies
cols_exlude <- c("Banks", "Consumer Finance", "Financial Admin.", "Insurance Brokers", "Mortgage Finance",
                 "Investment Services", "Specialty Finance", "Venture Capital Trust", "Private Equity",
                 "Real Estate Hold, Dev", "Reinsurance", "Life Insurance", "Asset Managers")

all_data <- all_data %>% filter(!(INDM %in% cols_exlude)) %>% select(-INDM)

# Following Hou et al.(2018) and excluding all micro stocks
# Create MV.USD.June to get yearly MV that matches with yearly accounting data
# MV.USD.June in Year y is for July year y until June y + 1
hlpvariable <- all_data %>% group_by(Id, year) %>% filter(month == 6)
hlpvariable <- hlpvariable %>% select(Id, year, MV.USD, MV) %>% rename(MV.USD.June = MV.USD, MV.June = MV)
all_data[,hcjun := ifelse(month>=7,year,year-1)]
all_data <- merge(all_data, hlpvariable, by.x = c("Id", "hcjun"), by.y = c("Id", "year"),
                  all.x = T)

# Define stocks into three size groups for each country
setorder(all_data, country, Date, -MV.USD.June)
hlpvariable <- all_data[month == 7 & !is.na(MV.USD.June)] %>% group_by(country, year) %>% 
  mutate(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                          ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big"))) %>% 
  select(country, year, pf.size, Id)

all_data <- merge(all_data,hlpvariable,
                 by.x=c("hcjun","Id", "country"),
                 by.y=c("year","Id", "country"),
                 all.x=T)

# Percentage of Micro Stocks in our sample
# How many unique micro stocks are in our sample
micro_stocks <- all_data %>% filter(pf.size == "Micro")
nrow(all_data %>% filter(pf.size == "Micro")) / nrow(all_data)

length(unique(micro_stocks$Id))

all_data <- all_data %>% filter(pf.size != "Micro")

# Merge FAT.monthly with the yearly accounting data
# We use the accounting data in year y to predict the returns from July year+1 to June year +2
all_data[,hcjun := ifelse(month>=7,year-1,year-2)]
all_data <- merge(all_data, FAT.yearly, by.x = c("Id", "hcjun"), by.y = c("Id", "YEAR"),
                  all.x = T)

# Lag monthly variables
all_data[, LMV.USD := shift(MV.USD, 1L), by=Id]
all_data[, LMV := shift(MV, 1L), by=Id]
all_data[, LMP := shift(UP, 1L), by=Id]

#lagAdjusetd Equity
hlpvariable <- all_data %>% group_by(Id, year) %>% filter(month == 6)%>%mutate(adjustedEquity=log(NOSH/AF))%>%
  ungroup()
hlpvariable <- as.data.table(hlpvariable)
hlpvariable[, lagAdjustedEquity := shift(adjustedEquity, 1L), by=Id]
all_data[,hcjun := ifelse(month>=7,year,year-1)]
all_data <- merge(all_data, hlpvariable[,c("Id", "year", "adjustedEquity", "lagAdjustedEquity")], by.x = c("Id", "hcjun"), by.y = c("Id", "year"),
                  all.x = T)

#Construct the factors to investigate
# Lag value for the monthly BM calculation
all_data[,hcjun := ifelse(month>=7,year-2,year-3)]
lag_variables <- FAT.yearly %>% select(Id, country, YEAR, WC02101, WC02999, WC02301,
                                       WC02201, WC02001, WC03101, WC03051,
                                       WC03063) %>% 
  rename(lag_inventories = WC02101, 
         lag_total_assets = WC02999, 
         lag_ppe = WC02301,
         LWC02201 = WC02201,
         LWC02001 = WC02001,
         LWC03101 = WC03101,
         LWC03051 = WC03051,
         LWC03063 = WC03063)
all_data <- merge(all_data, lag_variables, by.x = c("Id", "hcjun"), by.y = c("Id", "YEAR"),
                  all.x = T)

# Beta calculation

median(all_data$MV.USD, na.rm = T)
mean(all_data$MV.USD, na.rm = T)
# How many large companies
quantile(all_data$MV.USD, probs=c(0.9,1), na.rm = T)

large <- all_data %>% filter(ym == "Jan 2018")
small_static <- FAT.static %>% select(Id, INDM, NAME)
large <- merge(large, small_static, by = "Id")
large <- large %>% mutate(MV.Total = sum(MV.USD, na.rm = T),
                          Weights = MV.USD/MV.Total) %>% select(Id, MV.USD, Weights)

# Delete all unnecessary dataframes
# Clear memory
rm(FAT.monthly, FAT.static, FAT.yearly, hlpvariable, lag_variables, small_static,
   micro_stocks, cols_exlude)

#test <- FAT.monthly %>% group_by(Id) %>% do(rollapplyr(12, cumsum(RET)))
