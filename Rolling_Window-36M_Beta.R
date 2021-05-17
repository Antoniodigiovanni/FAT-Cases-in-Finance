library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(broom)

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

# We start in 1994
Market_Portfolio <- Market_Portfolio %>% filter(ym >= "Jan 1994")
beta_data <- FAT.monthly %>% filter(ym >= "Jan 1994" & !is.na(RET.USD)) %>% 
  select(Id, country, Date, MV.USD, RET.USD, ym)

beta_data <- merge(beta_data, Market_Portfolio,
                   by.x = c("country", "ym"),
                   by.y = c("Country", "ym"))

# We only include stocks that have more than 36 months of data because we are using 36M rolling window approach
M36_Data <- beta_data %>% group_by(Id) %>% summarize(count = n()) %>% filter(count >= 36)
beta_data <- merge(beta_data, M36_Data, by = "Id")

#coefs <- beta_data %>% group_by(Id) %>% rollapply(beta_data, width = 36, 
                                                  #FUN = function(z) coef(lm(RET.USD~RMRF, data = beta_data)),
                                                  #by.column = FALSE, align = "right")
#beta_data$year <- floor_date(beta_data$Date, "3year")

#test <- beta_data %>% slice(1:10000)
Coef <- . %>% as.data.frame %>% lm %>% coef
coefs <- beta_data %>% group_by(Id) %>% do(cbind(reg_col = select(., RET.USD, RMRF) %>% 
                                              rollapplyr(36, Coef, by.column = FALSE, fill = NA),
                                            date_col = select(., Date))) %>% 
  ungroup




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
j=0
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
  j=j+1
  print(j)
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

Beta_36_M$Id <- factor(Beta_36_M$Id, levels = levels(FAT.monthly$Id))

#save(Beta_36_M, file = "Beta_36_M.RData")

##### Merge Betas into FAT.Monthly ####
Beta_36_M <- Beta_36_M %>% mutate(
  Beta.year = year(ym)
) %>% select(-ym)


FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly[, Beta.year:=ifelse(month>=6, year, year-1)]

Test <- left_join(
  FAT.monthly,
  Beta_36_M,
  by = c("Id","Beta.year")
)

