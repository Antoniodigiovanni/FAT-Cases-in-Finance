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

# First drop the rows with NAs in MV, MV.USD, RET.USD to make the dataframe smaller
FAT.monthly <- FAT.monthly %>% drop_na(MV, MV.USD, RET.USD)

# Drop the rows that only have NA Values for the WC columns
# ncol gives total amount of columns - 5 for the first 5 columns that have values for every row but are not of interest
FAT.yearly <- FAT.yearly[rowSums(is.na(FAT.yearly)) != ncol(FAT.yearly) - 5,]


WC_Variables <- read_csv("FAT_yearly_renamed.csv", skip = 1)
#Change column names of the FAT.yearly dataframe
FAT.yearly_renamed <- FAT.yearly
names(FAT.yearly_renamed) <- names(WC_Variables)
FAT.yearly_renamed <- rename(FAT.yearly_renamed, Id = X1, country = X2, ICBSUC = X3, YEAR = X5)

rm(WC_Variables)

#Merge the FAT.yearly data with the monthly data
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly[,hcjun := ifelse(month>=7,year,year-1)]

hlpvariable <- FAT.monthly %>% drop_na(MV.USD) %>%  group_by(Id, year) %>% filter(month == 6)
hlpvariable <- hlpvariable %>% select(Id, year, MV.USD) %>% rename(MV.USD.June = MV.USD)
FAT.monthly <- merge(FAT.monthly, hlpvariable, by.x = c("Id", "hcjun"), by.y = c("Id", "year"), all.x = T)

# Create the big/small portfolios for every country
hkg <- FAT.monthly %>% filter(country == "HKG")
pf_size_hkg <- hkg[month==7 & !is.na(MV.USD.June),
                       .(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                                          ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big")),Id),
                       by=year]

panel_hkg <- merge(hkg,pf_size_hkg,
                   by.x=c("hcjun","Id"),
                   by.y=c("year","Id"),
                   all.x=T)

panel_hkg <- panel_hkg %>% drop_na(pf.size) %>% filter(pf.size %in% c("Big", "Small"))

rm(pf_size_hkg, hkg)

sgp <- FAT.monthly %>% filter(country == "SGP")
pf_size_sgp <- sgp[month==7 & !is.na(MV.USD.June),
                   .(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                                      ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big")),Id),
                   by=year]

panel_sgp <- merge(sgp,pf_size_sgp,
                   by.x=c("hcjun","Id"),
                   by.y=c("year","Id"),
                   all.x=T)

panel_sgp <- panel_sgp %>% drop_na(pf.size) %>% filter(pf.size %in% c("Big", "Small"))

rm(pf_size_sgp, sgp)

kor <- FAT.monthly %>% filter(country == "KOR")
pf_size_kor <- kor[month==7 & !is.na(MV.USD.June),
                   .(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                                      ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big")),Id),
                   by=year]

panel_kor <- merge(kor,pf_size_kor,
                   by.x=c("hcjun","Id"),
                   by.y=c("year","Id"),
                   all.x=T)

panel_kor <- panel_kor %>% drop_na(pf.size) %>% filter(pf.size %in% c("Big", "Small"))

rm(pf_size_kor, kor)

twn <- FAT.monthly %>% filter(country == "TWN")
pf_size_twn <- twn[month==7 & !is.na(MV.USD.June),
                   .(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                                      ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big")),Id),
                   by=year]

panel_twn <- merge(twn,pf_size_twn,
                   by.x=c("hcjun","Id"),
                   by.y=c("year","Id"),
                   all.x=T)

panel_twn <- panel_twn %>% drop_na(pf.size) %>% filter(pf.size %in% c("Big", "Small"))

rm(pf_size_twn, twn)


rm(hlpvariable)

#Now combine all the big stocks from the different countries
panel_all_countries <- rbind(panel_hkg, panel_kor, panel_sgp, panel_twn)
panel_all_countries <- panel_all_countries %>% filter(pf.size == "Big")

rm(panel_hkg, panel_kor, panel_sgp, panel_twn)

#Now we want to add the different factors to the panel data
#For the yearly accounting data we use the data in year -1 to predict the returns from July of year y to June in year y + 1
panel_all_countries[, hcjun := ifelse(month>=7, year-1, year-2)]

#Calculate value variables with the data from FAT.yearly, dont forget to divide by MV from FAT.monthly later and price!
# Operating accruals = OA deflated by total assets, not sure what deflated means in this context
# Dont understand the profitability variables
variables <- FAT.yearly %>% mutate(book_value = WC03501 + WC03263,
                                         earnings = WC01551,
                                         cash_flow = WC04860,
                                   ROE = WC01551 / book_value,
                                   ROA = WC01551 / WC02999,
                                   GP_A = (WC01001 - WC01501) / WC02999,
                                   OP_BE = (WC01001 - WC01051 - WC01101 - WC01251) / book_value,
                                   OA = (WC02201 - WC02001 - WC03101 + WC03051 + 
                                           ifelse(WC03063=="NA",0,WC03063) - ifelse(WC01151=="NA",0,WC01151)),
                                   NOA = (WC02999 - WC02001) - (WC02999 - WC03255 - WC03426 - WC03995)
                                   ) %>% select(Id, country, YEAR,
                                                book_value, earnings, cash_flow,
                                                ROE, ROA, GP_A,
                                                OP_BE, OA, NOA)


panel_all_countries <- merge(panel_all_countries, variables, by.x=c("Id","hcjun"), by.y=c("Id","YEAR"), all.x=T)
#Create B/M and E/P yearly and monthly
#Not sure if UP is the right price please check
#Commonly P/E is used Hanauer is using E/P does it matter?
panel_all_countries <- panel_all_countries %>% mutate(bm_yearly = book_value / (MV.USD.June*1000) ,
                                                      bm_monthly = book_value / (MV.USD*1000),
                                                      pe_monthy = UP / earnings,
                                                      cp_monthly = cash_flow / UP)


#Define investment variables
investment_variables <- FAT.yearly %>% group_by(Id) %>% 
  mutate(ag_y = WC02999, ag_y_1 = lag(WC02999)) %>% 
  select(Id, country, YEAR, ag_y, ag_y_1)

# Merge the variables with the monthly FAT Data but take into account the time lag




#Redo this but only with the needed calculated columns
#FAT.ALL <- merge(FAT.monthly, FAT.yearly,
                 #by.x=c("Id", "year"),
                 #by.y=c("Id", "YEAR"))


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
