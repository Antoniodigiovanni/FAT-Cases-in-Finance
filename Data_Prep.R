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
#Not removing NAs now as we may need to lag one row (to calculate some factors)
#FAT.monthly <- FAT.monthly %>% drop_na(RET.USD, MV.USD)
#Plot the return data to check for errors in data, this takes a while
#Maybe only plot the highest and lowest returns to check for obvious errors
biggest_losers <- FAT.monthly %>% arrange(RET.USD) %>% slice(1:1000)

ggplot(data = biggest_losers) +
  geom_point(mapping = aes(x = MV.USD, y = RET.USD))
biggest_winners <- FAT.monthly %>% 
  arrange(desc(RET.USD)) %>% 
  slice(1:1000)

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

# Included only necessary columns of FAT.yearly to reduce the size of all_data
all_data <- merge(FAT.monthly, 
                  FAT.yearly[,c("Id","YEAR","WC03501","WC03263","WC01551","WC04860",
                               "WC02999","WC01001","WC01501","WC01101","WC01251",
                               "WC02001","WC02201","WC03101","WC03063","WC03255",
                               "WC03426","WC03995","WC02301","WC02101","WC01051")],
                  by.x = c("Id", "hcjun"), 
                  by.y = c("Id", "YEAR"),
                  all.x = T)

# Create MV.USD.June to get yearly MV that matches with yearly accounting data
# MV.USD.June in Year y is for July year y until June y + 1
hlpvariable <- all_data %>% 
  #group_by(Id, year) %>% # I don't think group_by is needed here
  filter(month == 6)

hlpvariable <- hlpvariable %>% 
  select(Id, year, MV, MV.USD) %>% 
  rename(MV.USD.June = MV.USD, MV.June = MV)

all_data[,hcjun := ifelse(month>=7,year,year-1)]
all_data <- merge(all_data, 
                  hlpvariable, 
                  by.x = c("Id", "hcjun"), 
                  by.y = c("Id", "year"),
                  all.x = T)
rm(hlpvariable)

summary(FAT.static)
sort(unique(FAT.static$INDM))
# Look out for financial companies (Financial Admin. and Specialty Finance)
# "Asset Managers", "Banks", "Consumer Finance", "Insurance", "Insurance Brokers"
# "Private Equity", "Reinsurance", "Venture Capital" ??

#Maybe do that on FAT.monthly at the beginning


#### Create variables we want to include in our analysis ####
# Data from Datastream is in Millions, Worldscope Data is in 1k
# Start with value, more to come
# negative values for Book Value dont make sense, drop these
# Also for some of the observations we have high BV and low MV, might be currency issue
# Using MV.June instead of MV.USD.June

factors <- all_data %>% 
  # Filtering cash flow and earnings measures, 
  # only positive values (as Hanauer, Lauterbach - 2019)
  mutate(BM = (WC03501 + ifelse(is.na(WC03263),0,WC03263)) / (MV.June*1000),
         BM_m = (WC03501 +ifelse(is.na(WC03263),0,WC03263)) / (MV*1000),
         ROE = WC01551 / (WC03501 + WC03263),
         ROA = WC01551 / WC02999,
         "GP/A" = (WC01001-WC01501)/WC02999,
         "OP/BE" = (ifelse((WC01001 >=0 | WC01051>=0 | WC01101>=0 | WC01251>=0), #Checks also for missing values
                      (ifelse(is.na(WC01001),0,WC01001) 
                      - ifelse(is.na(WC01051),0,WC01051)
                      - ifelse(is.na(WC01101),0,WC01101) 
                      - ifelse(is.na(WC01251),0,WC01251))/(WC03501+WC03263),NA)),
         "E/P" = (ifelse(WC01551 >= 0, WC01551, NA)/ UP),
         "C/P" = (ifelse(WC04860 >= 0, WC04860, NA)/ UP),
         # Calculate yearlly increase characteristics like OA, NOA, AG, ...
         ) %>% 
  select(
    Id, Date, month, year, country, #country.x 
    MV,MV.June,MV.USD, MV.USD.June, RET.USD, ym,
    BM, BM_m, ROE, ROA, "GP/A", "OP/BE", "E/P", "C/P"
    ) #%>% 
  #rename(country = country.x) %>% 
  #drop_na(MV.USD.June)

Yearly_factors_list = c("BM","ROE","ROA","GP/A","OP/BE","E/P","C/P")
# Maybe we can use a dummy variable for when the BM ratio is negative? (only for Regressions)
# TODO
# Can we drop the NAs now?
factors <- factors %>% 
  drop_na(BM) %>% 
  filter(BM>0)

summary(factors)

# Define stocks into three size groups for each country
setorder(factors, country, Date, -MV.USD.June)
hlpvariable <- factors[month == 7 & !is.na(MV.USD.June)] %>% 
  group_by(country, year) %>%
  mutate(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                          ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big"))) %>% 
  select(country, year, pf.size, Id)

factors[,hcjun := ifelse(month>=7,year,year-1)]

factors <- merge(factors,hlpvariable,
                 by.x=c("hcjun","Id", "country"),
                 by.y=c("year","Id", "country"),
                 all.x=T)

rm(hlpvariable)
# Percentage of Micro Stocks in our sample
nrow(factors %>% filter(pf.size == "Micro")) / nrow(factors)
# Exclude the Micro Stocks
factors <- factors %>% filter(pf.size != "Micro")

ggplot(data = factors) + 
  geom_point(mapping = aes(x = MV.USD.June*1000, y = BM))
# There are some outliers, think about deleting them from the data

# 12-months return factor (Momentum)
#TODO


### Portfolio Sorts ####
# Now comes the portfolio sorts
# Determine the B/M breakpoints based on big stocks only

# ORIGINAL CODE
# hlpvariable2 <- factors[month == 7 & !is.na(BM) & pf.size == "Big"] %>% 
#   group_by(country, year) %>% 
#   summarize(bm_bb30 = quantile(BM, probs = c(0.3), na.rm = T),
#             bm_bb70 = quantile(BM, probs = c(0.7), na.rm = T)) %>% 
#   select(year, country, bm_bb30, bm_bb70)
# 
# factors <- merge(factors,hlpvariable2,
#                        by.x=c("hcjun", "country"),
#                        by.y=c("year", "country"),
#                        all.x=T)
# 
# factors[ , pf.bm := ifelse(BM>bm_bb70,"High",
#                                  ifelse((BM<=bm_bb70 & BM>bm_bb30),"Neutral",
#                                         ifelse(BM<=bm_bb30,"Low",NA)))]
# 
# factors[, SIZE_VALUE := paste0(pf.size,".",pf.bm)]

Factor_Sort <- function(df, factor){
  require(dplyr)
  setDT(df)
  H <- paste0(factor, "_bb70")
  L <- paste0(factor, "_bb30")
  Sort_breakpoints <- df[month == 7 & !is.na(df[[factor]]) & pf.size == "Big"] %>% 
    dplyr::group_by(country, year) %>% 
    dplyr::summarise(!!sym(L) := quantile(!!sym(factor), probs = c(0.3), na.rm = T),
                     !!sym(H) := quantile(!!sym(factor), probs = c(0.7), na.rm = T)) %>% 
    select(year,country, !!sym(L), !!sym(H) ) 
  
  df <- merge(df,Sort_breakpoints,
                   by.x=c("hcjun", "country"),
                   by.y=c("year", "country"),
                   all.x=T)
  
  Column <- paste0("pf.", factor)
  Sorted_Column <- df %>% mutate(
    !!paste0("pf.", factor) := ifelse(!!sym(factor)>!!sym(H),
                                   "High",
                                   ifelse((!!sym(factor) <= !!sym(H))
                                          & !!sym(factor) > !!sym(L),
                                          "Neutral",
                                          ifelse(!!sym(factor)<=!!sym(L),
                                                 "Low",
                                                 NA)))) %>% 
    select(Id, country, Date, !!paste0("pf.", factor))
  
 
  return(Sorted_Column)
} 

Sorted <- data.table(
  "Id" = factors$Id,
  "country" = factors$country,
  "Date" = factors$Date,
  "RET.USD" = factors$RET.USD
)

for (fact in Yearly_factors_list) {
  
  Sorted_column <- Factor_Sort(factors, fact)
  
  Sorted <- left_join(Sorted,
                      Sorted_column,
                      by=c("Id","Date","country"))
  
}
# TODO
# Calculate portfolio returns for each factor, then calculate returns 
# for hedge portfolios

### Portfolio Returns ####
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

+create_quintiles <- function(data, factor) {
  hlpvariable2 <- data[month == 7 & !is.na(data[[factor]]) & pf.size == "Big"] %>% group_by(country, year) %>% 
    summarize(bb20 = quantile(.data[[factor]], probs = c(0.2), na.rm = T),
              bb40 = quantile(.data[[factor]], probs = c(0.4), na.rm = T),
              bb60 = quantile(.data[[factor]], probs = c(0.6), na.rm = T),
              bb80 = quantile(.data[[factor]], probs = c(0.8), na.rm = T)) %>% 
    select(year, country, bb20, bb40, bb60, bb80)
  factors <- merge(data,hlpvariable2,
                   by.x=c("hcjun", "country"),
                   by.y=c("year", "country"))
}

# Assign each stock to the respective quintile
create_breakpoints <- function(data, factor) {
  data[ , pf.bm := ifelse(data[[factor]]>bb80, "Big",
                          ifelse((data[[factor]]<=bb80 & data[[factor]]>bb60),"LessBig",
                                 ifelse((data[[factor]]<=bb60 & data[[factor]]>bb40),"Neutral",
                                        ifelse((data[[factor]]<=bb40 & data[[factor]]>bb20),"LessSmall",
                                               ifelse(data[[factor]]<=bb20,"Small",NA)))))]
  
}

# Calculate the value-weighted returns for the different factor portfolios
calculate_factor_returns <- function(data, empty_df, factor) {
  portfolio_returns <- data[!is.na(pf.bm)] %>% 
    group_by(Date, pf.bm) %>% 
    summarize(ret.port = weighted.mean(RET.USD, MV.USD.June)) %>% 
    spread(pf.bm, ret.port) %>% mutate(hedge.pf = Big - Small) %>% 
    rename("5" = Big, "4" = LessBig, "3" = Neutral,"2" = LessSmall, "1" =  Small, "5-1" = hedge.pf) %>% 
    select(Date, "1", "2", "3", "4", "5", "5-1")
  portfolio_returns <- as.data.table(portfolio_returns)
  factor_returns <- colSums(portfolio_returns[,2:7], na.rm = T) / nrow(portfolio_returns)
  empty_df <- rbind(empty_df, factor_returns)
  empty_df
}

create_portfolio_sorts <- function(data, factor, empty_df) {
  factor_return <- create_quintiles(data, factor)
  factor_return <- create_breakpoints(factor_return, factor)
  empty_df <- calculate_factor_returns(factor_return, empty_df, factor)
}

# Create empty dataframe to display results
cols = c("1", "2", "3", "4", "5", "5-1")
portfolio_returns <- data.frame(matrix(nrow = 0, ncol = length(cols)))

# Exclude the stocks that have negative earnings, cash flows or gross profits
# We start with Book-To-Market and we only consider stocks with positive BM
bm_factor <- factors %>% filter(BM>0)
portfolio_returns <- create_portfolio_sorts(bm_factor, "BM", portfolio_returns)
# Filter out stocks with negative earnings 
ep_factor <- factors %>% filter(EP > 0) %>% drop_na(EP)
portfolio_returns <- create_portfolio_sorts(ep_factor, "EP", portfolio_returns)
# Filter out stocks with negative CF
cp_factor <- factors %>% filter(CP > 0) %>% drop_na(CP)
portfolio_returns <- create_portfolio_sorts(cp_factor, "CP", portfolio_returns)

roe_factor <- factors %>% filter(ROE > 0)
portfolio_returns <- create_portfolio_sorts(roe_factor, "ROE", portfolio_returns)

roa_factor <- factors %>% filter(ROA > 0)
portfolio_returns <- create_portfolio_sorts(roa_factor, "ROA", portfolio_returns)

gpa_factor <- factors %>% filter(GPA > 0)
portfolio_returns <- create_portfolio_sorts(gpa_factor, "GPA", portfolio_returns)

opbe_factor <- factors %>% filter(OPBE > 0)
portfolio_returns <- create_portfolio_sorts(opbe_factor, "OPBE", portfolio_returns)

oa_factor <- factors %>% drop_na(OA)
portfolio_returns <- create_portfolio_sorts(oa_factor, "OA", portfolio_returns)

colnames(portfolio_returns) <- cols
rows <- c("BM", "EP", "CP", "ROE", "ROA", "GP/A", "OP/BE", "OA")
rownames(portfolio_returns) <- rows
