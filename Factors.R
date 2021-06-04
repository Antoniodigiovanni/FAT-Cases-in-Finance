library(rstudioapi)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")

# Merge Beta in all_data - Correct the fact that Beta is seen as a list even
# though it is a column in the df (comes from the original calculation)
load("Beta_36_M.RData")
#Beta_36_M$Id <- factor(Beta_36_M$Id, levels = levels(all_data$Id))

Beta_36_M <- Beta_36_M %>% mutate(
  Beta.year = year(ym)
) %>% select(-ym)

Beta_36_M$Beta <- unlist(Beta_36_M$Beta)
Beta_36_M$Id <- unlist(Beta_36_M$Id)

all_data <- all_data[, Beta.year:=ifelse(month>=6, year, year-1)] # Should we update in July?



# Why there are NULL values and not NA?
all_data <- left_join(
  all_data,
  Beta_36_M,
  by = c("Id","Beta.year")
) %>% select(-Beta.year)

rm(Beta_36_M)

# Dont we have to lag the monthly values?
# OPBE  Ratio is WRONG
# We have to first set the NA Values to 0 and then we can use it in the calculation
factors <- all_data
# Drop the observations that have NA for RET since this is our dependent variable in the Regressions
factors <- factors %>% drop_na(RET)
# Now we set the Worldscope variables to zero if they are NA
factors <- factors %>% replace(is.na(.), 0)

factors <- factors %>% mutate(
  BM = (WC03501+WC03263) / (MV.June*1000),
  BM_m = (WC03501+WC03263) / (LMV*1000),
  bm_dummy = ifelse(BM<0, 1, 0),
  bm_m_dummy = ifelse(BM_m < 0, 1, 0),
  EP = WC03501 / (MV.June*1000 / NOSH),
  EP_m = WC03501 / LMP,
  earnings_dummy = ifelse(EP < 0, 1, 0),
  earnings_m_dummy = ifelse(EP_m<0, 1, 0),
  CP = WC04860 / (MV.June*1000 / NOSH),
  CP_m = WC04860 / LMP,
  cashflow_dummy = ifelse(CP < 0, 1, 0),
  cashflow_m_dummy = ifelse(CP_m<0, 1, 0),
  ROE = WC01551 / (WC03501 +WC03263),
  ROA = (WC01551 / WC02999), 
  GPA =  (WC01001 - WC01501) / WC02999,
  profits_dummy = ifelse(GPA < 0, 1, 0),
  OPBE = (WC01001 - WC01051 - WC01101 - WC01251) / (WC03501+WC03263),
  #OPBE = (ifelse(!is.na(WC01051)| !is.na(WC01101) | !is.na(WC01251),
                 #ifelse(is.na(WC01001),0,WC01001) - ifelse(is.na(WC01051),0,WC01051) - ifelse(is.na(WC01101),0,WC01101) - ifelse(is.na(WC01251),0,WC01251) / (WC03501 + ifelse(is.na(WC03263),0,WC03263)),NA)),
  op_dummy = ifelse(OPBE<0, 1, 0),
  # Deflated means divided or subtracted?
  OA = ((((WC02201 - LWC02201) - (WC02001 - LWC02001) - (WC03101 - LWC03101) + 
           (WC03051 - LWC03051) + ifelse(!is.na(WC03063) & !is.na(LWC03063),
                                         (WC03063 - LWC03063),0)) - ifelse(is.na(WC01151),
                                                                           0,WC01151))/WC02999),
    # (WC02201 - WC02001 - WC03101 + WC03051 + 
    #       ifelse(WC03063=="NA",0,WC03063) - ifelse(WC01151=="NA",0,WC01151)),
  OL = WC02999 - WC03255 - WC03426 - WC03995,
  NOA = ((WC02999 - WC02001) - OL)/lag_total_assets,
  noa_dummy = ifelse(NOA<0, 1, 0),
  oa_dummy = ifelse(OA<0, 1, 0),
  #NOA = OA - OL - lag_total_assets,
  AG = ((WC02999 / lag_total_assets)-1),
  # NSI =
  # CEI = To calculate
  ItA = (WC02301 - lag_ppe + WC02101 - lag_inventories) / lag_total_assets,
  EPS = WC05202,
  eps_dummy = ifelse(WC05202<0, 1, 0),
  TEY = WC05202/(MV.June / NOSH),
  tey_dummy = ifelse(TEY<0, 1, 0),
  BookToEV = WC05491/WC18100,
  DtoE = WC03255 /WC03501,
  QuickRatio = (WC02201 - WC02101) / WC03101,
  NSI=adjustedEquity-lagAdjustedEquity
) %>% 
  select(Id, country.x, Date, month, year, LMV, LMV.USD, MV.USD, MV.USD.June, RET.USD, RET, Beta, ym,
         BM, BM_m, bm_dummy, bm_m_dummy, EP, EP_m, earnings_dummy, earnings_m_dummy, CP, CP_m, cashflow_dummy, cashflow_m_dummy, ROE, ROA, GPA, profits_dummy,
         OPBE, op_dummy, OA, OL, NOA, AG, ItA, NSI,
         EPS, eps_dummy, TEY, tey_dummy, BookToEV, DtoE, QuickRatio, noa_dummy, oa_dummy, pf.size, hcjun) %>% 
  rename(country = country.x) %>% drop_na(MV.USD.June)

# Include FFtF and we use EBITDA - EBIT to get depreciation amount

#Momentum
Momentum <- all_data %>% group_by(Id) %>% mutate(RET.adj = RET/100 + 1) %>% 
  do(cbind(reg_col = select(., RET.adj) %>% 
             rollapplyr(list(seq(-12, -2)), prod, by.column = FALSE, fill = NA),
           date_col = select(., Date))) %>% 
  ungroup() %>% rename("Momentum" = reg_col)

Momentum <- Momentum %>% mutate(Momentum = (Momentum-1))

Momentum_short <- all_data %>% group_by(Id) %>% mutate(RET.adj = RET/100 + 1) %>% 
  do(cbind(reg_col = select(., RET.adj) %>% 
             rollapplyr(list(seq(-6, -2)), prod, by.column = FALSE, fill = NA),
           date_col = select(., Date))) %>% 
  ungroup() %>% rename("Momentum_short" = reg_col)

Momentum_short <- Momentum_short %>% mutate(Momentum_short = (Momentum_short-1))

factors <- left_join(factors,
                     Momentum,
                     by=c("Id", "Date"))

factors <- left_join(factors,
                     Momentum_short,
                     by=c("Id", "Date"))

rm(Momentum)
# Complete list of Factors passed to other scripts
# Add "Beta" to the list once fixed (now it is a list embedded in a df and errors arise from this)
Yearly_factors_list = (c("Beta", "BM", "EP", "CP", "ROE", "ROA", "GPA", "OPBE", "OA",
                          "OL", "NOA", "AG", "ItA","EPS", "TEY", "BookToEV", 
                          "DtoE", "QuickRatio", "NSI"))

Monthly_factors_list = c("CP_m", "BM_m", "EP_m", "Momentum")




# Calculating the correlation matrix of factors

Correlation_Matrix <- factors %>% select(-Id,-Date,-country,-month,-year,
                                         -bm_dummy,-bm_m_dummy,-earnings_dummy,
                                         -cashflow_dummy,-profits_dummy,-op_dummy,
                                         eps_dummy,tey_dummy,-noa_dummy,-pf.size,
                                         -oa_dummy,-hcjun,-ym, -eps_dummy)

cor <- cor(Correlation_Matrix, use = "na.or.complete")
as.data.table(cor) -> cor


rm(cor, Correlation_Matrix)
