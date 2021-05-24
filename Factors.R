source("Real_data_prep.R")


factors <- all_data %>% mutate(
  BM = (WC03501+WC03263) / (MV.June*1000),
  BM_m = (WC03501+WC03263) / (MV*1000),
  EP = WC03501 / (MV.June*1000 / NOSH),
  EP_m = WC03501 / (MV*1000 / NOSH),
  CP = WC04860 / (MV.June*1000 / NOSH),
  CP_m = WC04860 / (MV*1000 / NOSH),
  ROE = WC01551 / (WC03501 + WC03263),
  ROA = (WC01551 / WC02999), 
  GPA =  (WC01001 - WC01501) / WC02999,
  OPBE = (WC01001 - WC01501 - WC01101 - WC01251) / (WC03501 + WC03263),
  OA = (WC02201 - WC02001 - WC03101 + WC03051 + 
          ifelse(WC03063=="NA",0,WC03063) - ifelse(WC01151=="NA",0,WC01151)),
  OL = WC02999 - WC03255 - WC03426 - WC03995,
  NOA = OA - OL - lag_total_assets,
  AG = WC02999 / (lag_total_assets*100),
  ItA = (WC02301 - lag_ppe + WC02101 - lag_inventories) / lag_total_assets,
  EPS = WC05202,
  TEY = WC05202/(MV.June / NOSH),
  BookToEV = WC05491/WC18100,
  DtoE = WC03255 /WC03501,
  QuickRatio = (WC02201 - WC02101) / WC03101,
  
) %>% 
  select(Id, country.x, Date, month, year, MV.USD, MV.USD.June, RET.USD, ym,
         BM, BM_m, EP, EP_m, CP, CP_m, ROE, ROA, GPA, OPBE, OA) %>% 
  rename(country = country.x) %>% drop_na(MV.USD.June)

# Include FFtF and we use EBITDA - EBIT to get depreciation amount
