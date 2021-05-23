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
load("Data/FAT_monthly.RData")
load("Data/FAT_static.RData")
# Yearly Accounting Data from Worldscope
load("Data/FAT_yearly.RData")

# Get data from year 1994 to ensure data quality
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly <- FAT.monthly %>% filter(year > 1993)

small_static <- FAT.static %>% select(Id, INDM)

all_data <- merge(FAT.monthly, small_static, by = "Id")
table(small_static$INDM)

# Banks, Consumer Finance, FInancial Admin., Insurance Brokers, Mortgage Finance, Investment Services,
# Specialty Finance, Venture Capital Trust, Private Equity, Real Estate Hold, Dev, Reinsurance,
# Life Insurance, Asset Managers
# Exclude financial companies
cols_exlude <- c("Banks", "Consumer Finance", "Financial Admin.", "Insurance Brokers", "Mortgage Finance",
                 "Investment Services", "Specialty Finance", "Venture Capital Trust", "Private Equity",
                 "Real Estate Hold, Dev", "Reinsurance", "Life Insurance", "Asset Managers")


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

all_data <- all_data %>% filter(pf.size != "Micro")

# Merge FAT.monthly with the yearly accounting data
# We use the accounting data in year y to predict the returns from July year+1 to June year +2
all_data[,hcjun := ifelse(month>=7,year-1,year-2)]
all_data <- merge(all_data, FAT.yearly, by.x = c("Id", "hcjun"), by.y = c("Id", "YEAR"),
                  all.x = T)


#Construct the factors to investigate
# Lag value for the monthly BM calculation
all_data[,hcjun := ifelse(month>=7,year-2,year-3)]
lag_variables <- FAT.yearly %>% select(Id, country, YEAR, WC02101, WC02999, WC02301) %>% 
  rename(lag_inventories = WC02101, lag_total_assets = WC02999, lag_ppe = WC02301)
all_data <- merge(all_data, lag_variables, by.x = c("Id", "hcjun"), by.y = c("Id", "YEAR"),
                  all.x = T)

# Beta calculation
# Delete all unnecessary dataframes
# momentum

#test <- beta_data %>% slice(1:10000)
test <- FAT.monthly %>% slice(1:250000)
Coef <- . %>% as.data.frame %>% cumsum %>% coef
coefs <- test %>% group_by(Id) %>% do(cbind(reg_col = select(., RET) %>% 
                                                   rollapplyr(list(seq(-12, -2)), sum, by.column = FALSE, fill = NA),
                                                 date_col = select(., Date))) %>% 
  ungroup

test <- FAT.monthly %>% group_by(Id) %>% do(rollapplyr(12, cumsum(RET)))
