library(rstudioapi)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")

# Merge Beta in all_data
load("Beta_36_M.RData")
Beta_36_M$Id <- factor(Beta_36_M$Id, levels = levels(all_data$Id))

Beta_36_M <- Beta_36_M %>% mutate(
  Beta.year = year(ym)
) %>% select(-ym)



all_data <- all_data[, Beta.year:=ifelse(month>=6, year, year-1)] # Should we update in July?

# Why there are NULL values and not NA?
all_data <- left_join(
  all_data,
  Beta_36_M,
  by = c("Id","Beta.year")
) %>% select(-Beta.year)

rm(Beta_36_M)

factors <- all_data %>% mutate(
  BM = (WC03501+ifelse(is.na(WC03263),0,WC03263)) / (MV.June*1000),
  BM_m = (WC03501+ifelse(is.na(WC03263),0,WC03263)) / (MV*1000),
  EP = WC03501 / (MV.June*1000 / NOSH),
  EP_m = WC03501 / (MV*1000 / NOSH),
  CP = WC04860 / (MV.June*1000 / NOSH),
  CP_m = WC04860 / (MV*1000 / NOSH),
  ROE = WC01551 / (WC03501 + ifelse(is.na(WC03263),0,WC03263)),
  ROA = (WC01551 / WC02999), 
  GPA =  (WC01001 - WC01501) / WC02999,
  OPBE = (ifelse(!is.na(WC01001) | !is.na(WC01051)| !is.na(WC01101) | !is.na(WC01251),
                 ifelse(is.na(WC01001),0,WC01001) - ifelse(is.na(WC01051),0,WC01051) - ifelse(is.na(WC01101),0,WC01101) - ifelse(is.na(WC01251),0,WC01251) / (WC03501 + ifelse(is.na(WC03263),0,WC03263)),NA)),
  # Deflated means divided or subtracted?
  OA = ((((WC02201 - LWC02201) - (WC02001 - LWC02001) - (WC03101 - LWC03101) + 
           (WC03051 - LWC03051) + ifelse(!is.na(WC03063) & !is.na(LWC03063),
                                         (WC03063 - LWC03063),0)) - ifelse(is.na(WC01151),
                                                                           0,WC01151))/WC02999),
    # (WC02201 - WC02001 - WC03101 + WC03051 + 
    #       ifelse(WC03063=="NA",0,WC03063) - ifelse(WC01151=="NA",0,WC01151)),
  OL = WC02999 - WC03255 - WC03426 - WC03995,
  NOA = ((WC02999 - WC02001) - OL)/lag_total_assets,
  #NOA = OA - OL - lag_total_assets,
  AG = ((WC02999 / lag_total_assets)-1)*100,
  # NSI =
  # CEI = To calculate
  ItA = (WC02301 - lag_ppe + WC02101 - lag_inventories) / lag_total_assets,
  EPS = WC05202,
  TEY = WC05202/(MV.June / NOSH),
  BookToEV = WC05491/WC18100,
  DtoE = WC03255 /WC03501,
  QuickRatio = (WC02201 - WC02101) / WC03101
) %>% 
  select(Id, country.x, Date, month, year, MV.USD, MV.USD.June, RET.USD, Beta, ym,
         BM, BM_m, EP, EP_m, CP, CP_m, ROE, ROA, GPA, OPBE, OA, OL, NOA, AG, ItA,
         EPS, TEY, BookToEV, DtoE, QuickRatio, pf.size, hcjun) %>% 
  rename(country = country.x) %>% drop_na(MV.USD.June)

# Include FFtF and we use EBITDA - EBIT to get depreciation amount



# Complete list of Factors passed to other scripts
Yearly_factors_list = (c( "Beta", "BM", "EP", "CP", "ROE", "ROA", "GPA", "OPBE", "OA",
                          "OL", "NOA", "AG", "ItA","EPS", "TEY", "BookToEV", 
                          "DtoE", "QuickRatio"))

Monthly_factors_list = c("CP_m", "BM_m", "EP_m")
