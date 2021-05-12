library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

load("FAT_monthly.RData")
load("FAT_static.RData")
load("FAT_yearly.RData")

WC_Variables <- read_csv("FAT_yearly_renamed.csv", skip = 1)
#Change column names of the FAT.yearly dataframe
names(FAT.yearly) <- names(WC_Variables)
FAT.yearly <- rename(FAT.yearly, Id = X1, country = X2, ICBSUC = X3, YEAR = X5)

#Merge the FAT.yearly data with the monthly data
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]

FAT.ALL <- merge(FAT.monthly, FAT.yearly,
                 by.x=c("Id", "year"),
                 by.y=c("Id", "YEAR"))

rm(WC_Variables)


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

Beta_Regr <- data.frame(
  "Id" = FAT.monthly$Id,
  "Country" = FAT.monthly$country,
  "ym" = FAT.monthly$ym,
  "Date" = FAT.monthly$Date,
  "RET.USD" = FAT.monthly$RET.USD)
  
Beta_Regr <- Beta_Regr[!is.na(Beta_Regr$RET.USD),  ]

Beta_Regr <- left_join(Beta_Regr,
                       Market_Portfolio[c("Country","ym","RMRF")],
                       by = c("Country","ym"))

Beta_Regr <- Beta_Regr %>%
  mutate(cut = as.Date(cut(Date, breaks = '36 months'))) #'36 months' #When changing time horizon change also line 138

# Shift cut from 1st of the month to last day of the month
day(Beta_Regr$cut) <- days_in_month(Beta_Regr$cut)


# Perform the regresssion to calculate Betas.
Beta.36.Months <- Beta_Regr %>% 
  group_by(Id, cut) %>%
  # Filter stocks that have been listed at least 12 months in the 36 months group
  filter(n() >= 12) %>%
  do(lm = lm(RET.USD~RMRF, data = .)) %>% 
  mutate(Intercept = summary(lm)$coeff[1],
         Beta = summary(lm)$coeff[2]) %>%
  select(-lm) %>%
  ungroup() %>% 
  rename(Beta_group = cut) %>% 
  mutate(Beta_group = as.yearmon(Beta_group))
  
# Assign 36-months Betas to the stocks in the 36-months time-frame

# Using different df for testing purposes (temporary)
FAT.monthly_beta <- FAT.monthly_beta %>% 
  mutate(cut = as.Date(cut(Date, breaks = '36 months')))

# Shift cut from 1st of the month to last day of the month
day(FAT.monthly_beta$cut) <- days_in_month(FAT.monthly_beta$cut)

FAT.monthly_beta <- FAT.monthly_beta %>% 
  mutate(cut = as.yearmon(cut)) %>% 
  rename(Beta_group = cut)

Beta.36.Months <- Beta.36.Months %>% select(-Intercept)
FAT.monthly_beta <- left_join(FAT.monthly_beta,
                         Beta.36.Months,
                         by = c("Id", "Beta_group"))


# Select only stocks that have Beta
#FAT.monthly_beta <- FAT.monthly_beta %>% 
#  drop_na(Beta)


### Plots ####

# Plot Monthly Market Returns of SGP
Market_Portfolio[Market_Portfolio$Country == "SGP", ] %>% 
  ggplot(aes(x = ym, y = RM)) +
  geom_line(color = "darkorchid4") +
  labs(title = "Monthly Market Returns",
       y = "Monthly Return",
       x = "Date") + 
  theme_bw(base_size = 15)

# Plot RF rate (selecting one country to avoid repetitions)
Market_Portfolio[Market_Portfolio$Country == "SGP", ] %>% 
  ggplot(aes(x = ym, y = RF)) +
  geom_line(color = "darkorchid4") +
  labs(title = "Monthly Market Returns",
       y = "Monthly Return",
       x = "Date") + 
  theme_bw(base_size = 15)
