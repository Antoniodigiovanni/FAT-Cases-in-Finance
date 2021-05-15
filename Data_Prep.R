library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")
  
#Data from Datastream
load("FAT_monthly.RData")
load("FAT_static.RData")
# Yearly Accounting Data from Worldscope
load("FAT_yearly.RData")

# How much missing data for the Return?
glimpse(FAT.monthly)
summary(FAT.monthly)
# Approx. 2.7Mio rows for MV/Ret are N/A
FAT.monthly <- FAT.monthly %>% drop_na(RET.USD, MV.USD)
#Plot the return data to check for errors in data, this takes a while
#Maybe only plot the highest and lowest returns to check for obvious errors
biggest_losers <- FAT.monthly %>% arrange(RET.USD) %>% slice(1:1000)
ggplot(data = biggest_losers) +
  geom_point(mapping = aes(x = MV.USD, y = RET.USD))
biggest_winners <- FAT.monthly %>% arrange(desc(RET.USD)) %>% slice(1:1000)
ggplot(data = biggest_winners) +
  geom_point(mapping = aes(x = MV.USD, y = RET.USD))

biggest_winners
rm(biggest_losers, biggest_winners)

# Maybe winsorize the 1% and 99% percentile
# Get data from year 1994 to ensure data quality
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly <- FAT.monthly %>% filter(year > 1993)

# Merge FAT.monthly with the yearly accounting data
# We use the accounting data in year y to predict the returns from July year+1 to June year +2
FAT.monthly[,hcjun := ifelse(month>=7,year-1,year-2)]
all_data <- merge(FAT.monthly, FAT.yearly, by.x = c("Id", "hcjun"), by.y = c("Id", "YEAR"),
      all.x = T)

# Create MV.USD.June to get yearly MV that matches with yearly accounting data
# MV.USD.June in Year y is for July year y until June y + 1
hlpvariable <- all_data %>% group_by(Id, year) %>% filter(month == 6)
hlpvariable <- hlpvariable %>% select(Id, year, MV.USD) %>% rename(MV.USD.June = MV.USD)
all_data[,hcjun := ifelse(month>=7,year,year-1)]
all_data <- merge(all_data, hlpvariable, by.x = c("Id", "hcjun"), by.y = c("Id", "year"),
                  all.x = T)
rm(hlpvariable)

summary(FAT.static)
sort(unique(FAT.static$INDM))
# Look out for financial companies (Financial Admin. and Specialty Finance)

# Create variables we want to include in our analysis
# Data from Datastream is in Millions, Worldscope Data is in 1k
# Start with value, more to come
# negative values for Book Value dont make sense, drop these
# Also for some of the observations we have high BV and low MV, might be currency issue
factors <- all_data %>% mutate(BM = (WC03501+WC03263) / (MV.USD.June*1000),
                                BM_m = (WC03501+WC03263) / (MV.USD*1000),
                                ROE = WC01551 / (WC03501 + WC03263),
                                ROA = WC01551 / WC02999) %>% select(
                                  Id, country.x, Date, month, year, MV.USD, MV.USD.June, RET.USD, ym,
                                  BM, BM_m, ROE, ROA
                                ) %>% rename(country = country.x) %>% drop_na(MV.USD.June)

factors <- factors %>% drop_na(BM) %>% filter(BM>0)

summary(factors)

# Define stocks into three size groups for each country
setorder(factors, country, Date, -MV.USD.June)
hlpvariable <- factors[month == 7 & !is.na(MV.USD.June)] %>% group_by(country, year) %>% 
  mutate(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                          ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big"))) %>% 
  select(country, year, pf.size, Id)

factors[,hcjun := ifelse(month>=7,year,year-1)]

factors <- merge(factors,hlpvariable,
                 by.x=c("hcjun","Id", "country"),
                 by.y=c("year","Id", "country"),
                 all.x=T)

# Percentage of Micro Stocks in our sample
nrow(factors %>% filter(pf.size == "Micro")) / nrow(factors)
# Exclude the Micro Stocks
factors <- factors %>% filter(pf.size != "Micro")

ggplot(data = factors) + 
  geom_point(mapping = aes(x = MV.USD.June*1000, y = BM))
# There are some outliers, think about deleting them from the data


# Now comes the portfolio sorts
# Determine the B/M breakpoints based on big stocks only
hlpvariable2 <- factors[month == 7 & !is.na(BM) & pf.size == "Big"] %>% group_by(country, year) %>% 
  summarize(bm_bb30 = quantile(BM, probs = c(0.3), na.rm = T),
         bm_bb70 = quantile(BM, probs = c(0.7), na.rm = T)) %>% select(year, country, bm_bb30, bm_bb70)

factors <- merge(factors,hlpvariable2,
                       by.x=c("hcjun", "country"),
                       by.y=c("year", "country"),
                       all.x=T)

factors[ , pf.bm := ifelse(BM>bm_bb70,"High",
                                 ifelse((BM<=bm_bb70 & BM>bm_bb30),"Neutral",
                                        ifelse(BM<=bm_bb30,"Low",NA)))]

factors[, SIZE_VALUE := paste0(pf.size,".",pf.bm)]

portfolio_returns <- factors[!is.na(pf.size) & !is.na(pf.bm)] %>% # this operator nests functions
  group_by(Date,SIZE_VALUE) %>% # do "everything" for the groups specified here
  summarize(ret.port = weighted.mean(RET.USD,
                                     MV.USD.June)) %>% # vw returns using lagged mcap
  spread(SIZE_VALUE,ret.port) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )

portfolio_returns <- as.data.table(portfolio_returns)
