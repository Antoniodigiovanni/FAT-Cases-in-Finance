library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(broom)

source("Factors.R")

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

# Include for dummy for negative earnings, cash flow and gross profits 
# First test the traditional Fama/French 5 Factors
cross_reg <- factors %>% select(Id, ym, country, RET.USD, Beta, BM, bm_dummy , OPBE, op_dummy , AG, MV.USD.June) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

# lm(RET.USD~., data=cross_reg[-c(Date)])
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                    beta=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                    bm=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                    opbe = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                    ag = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                    size = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                        no.obs=length(Id)),by=ym]


ffm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic))


# Include Momentum to the model
cross_reg <- factors %>% select(Id, ym, RET.USD, Beta, BM, bm_dummy, OPBE, op_dummy,
                                AG, MV.USD.June, Momentum, country) %>% drop_na(.) %>% 
  filter(AG != "Inf")
# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  no.obs=length(Id)),by=ym]


sfm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Comparison Value Variables
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM, BM_m, bm_dummy, bm_m_dummy,
         OPBE, op_dummy, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0
cross_reg[BM_m<0]$BM_m <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[3],
                                  bm_m=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[4],
                                  opbe = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[5],
                                  ag = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[6],
                                  size = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[7],
                                  mom = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[8],
                                  no.obs=length(Id)),by=ym]


value1 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Drop yearly BM
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, bm_m_dummy, EP_m, earnings_dummy, CP_m, cashflow_dummy,
         OPBE, op_dummy, AG, MV.USD.June, Momentum,EPS, eps_dummy,
         TEY, tey_dummy, DtoE, BookToEV, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[EP_m<0]$EP_m <- 0
cross_reg[CP_m<0]$CP_m <- 0
cross_reg[EPS<0]$EPS <- 0
cross_reg[TEY<0]$TEY <- 0


CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[3],
                                  ep_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[4],
                                  cp_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[5],
                                  eps=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[6],
                                  tey=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[7],
                                  dtoe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[8],
                                  btev=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[9],
                                  opbe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[10],
                                  ag=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[11],
                                  size=lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[12],
                                  mom = lm(RET.USD~Beta+BM_m+EP_m+CP_m+EPS+TEY+DtoE+BookToEV+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+eps_dummy+tey_dummy)$coefficient[13],
                                  no.obs=length(Id)),by=ym]



value2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                     CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                     EPS = c(CS.reg.estimates[,t.test(eps)]$estimate, CS.reg.estimates[, t.test(eps)]$statistic),
                     TEY = c(CS.reg.estimates[,t.test(tey)]$estimate, CS.reg.estimates[, t.test(tey)]$statistic),
                     DtoE = c(CS.reg.estimates[,t.test(dtoe)]$estimate, CS.reg.estimates[, t.test(dtoe)]$statistic),
                     BookToEV = c(CS.reg.estimates[,t.test(btev)]$estimate, CS.reg.estimates[, t.test(btev)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Drop EPM and run again

# Comparison Profitability Variables
# Can OA be negative?
# ROA, ROE, OL?
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, bm_m_dummy, EP_m, earnings_dummy,
         CP_m, cashflow_dummy, ROE, ROA, GPA, profits_dummy, OPBE, op_dummy,
         NOA, noa_dummy, OA, oa_dummy, OL, AG, MV.USD.June, Momentum, NSI, ItA, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[EP_m<0]$EP_m <- 0
cross_reg[CP_m<0]$CP_m <- 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[NOA<0]$NOA <- 0
cross_reg[OA<0]$OA <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[3],
                                  ep_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[4],
                                  cp_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[5],
                                  roe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[6],
                                  roa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[7],
                                  gpa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[8],
                                  opbe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[9],
                                  noa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[10],
                                  oa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[11],
                                  ol=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[12],
                                  ag=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[13],
                                  size=lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[14],
                                  mom = lm(RET.USD~Beta+BM_m+EP_m+CP_m+ROE+ROA+GPA+OPBE+NOA+OA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy+oa_dummy)$coefficient[15],
                                  no.obs=length(Id)),by=ym]

profitability <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                     CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                     ROE = c(CS.reg.estimates[,t.test(roe)]$estimate, CS.reg.estimates[, t.test(roe)]$statistic),
                     ROA = c(CS.reg.estimates[,t.test(roa)]$estimate, CS.reg.estimates[, t.test(roa)]$statistic),
                     GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                     OA = c(CS.reg.estimates[,t.test(oa)]$estimate, CS.reg.estimates[, t.test(oa)]$statistic),
                     OL = c(CS.reg.estimates[,t.test(ol)]$estimate, CS.reg.estimates[, t.test(ol)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Drop ROE, ROA and OA to check if OL is still insignificant since it had high tstat in the sorts
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  ep_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  cp_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  gpa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  
                                  noa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  
                                  ol=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  ag=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  size=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  mom =lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+NOA+OL+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[11],
                                  no.obs=length(Id)),by=ym]

profitability2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                            BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                            EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                            CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                            
                            GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                            
                            NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                            
                            OL = c(CS.reg.estimates[,t.test(ol)]$estimate, CS.reg.estimates[, t.test(ol)]$statistic),
                            AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                            Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                            Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Drop EP_m, OL
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  cp_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  gpa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  noa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  ag=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  size=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  mom =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  no.obs=length(Id)),by=ym]

profitability3 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                             BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                             CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                             GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                             NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                             AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                             Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                             Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Investment
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  cp_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  gpa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  noa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  ag=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  size=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  mom =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  nsi =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  ita =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+AG+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[11],
                                  no.obs=length(Id)),by=ym]
inv1 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                             BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                             CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                             GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                             NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                             AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                             Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                             Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic),
                            NSI = c(CS.reg.estimates[,t.test(nsi)]$estimate, CS.reg.estimates[, t.test(nsi)]$statistic),
                           ItA= c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic))

# drop AG due to insignificance
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  cp_m=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  gpa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  noa=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  size=lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  mom =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  nsi =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  ita =lm(RET.USD~Beta+BM_m+CP_m+GPA+NOA+MV.USD.June+Momentum+NSI+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  no.obs=length(Id)),by=ym]

inv2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                   BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                   CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                   GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                   NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                   Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                   Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic),
                   NSI = c(CS.reg.estimates[,t.test(nsi)]$estimate, CS.reg.estimates[, t.test(nsi)]$statistic),
                   ItA= c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic))

#Strongest vs 5-Factor
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM, bm_dummy, BM_m, bm_m_dummy,
         CP_m, cashflow_dummy, GPA, profits_dummy, OPBE, op_dummy,
         NOA, noa_dummy, AG, MV.USD.June, Momentum, NSI, ItA, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[CP_m<0]$CP_m <- 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[NOA<0]$NOA <- 0


CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                bm_m=lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  gpa=lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  cpm = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  noa = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  momentum = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  nsi = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  ita = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  beta = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  bm = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  opbe = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[11],
                                ag = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[12],
                                size = lm(RET.USD~BM_m+GPA+CP_m+NOA+Momentum+NSI+ItA+Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[13],
                                   no.obs=length(Id)),by=ym]


strongVSffm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM_m = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                  GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                  CPM = c(CS.reg.estimates[,t.test(cpm)]$estimate, CS.reg.estimates[, t.test(cpm)]$statistic),
                  NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                  Momentum = c(CS.reg.estimates[,t.test(momentum)]$estimate, CS.reg.estimates[, t.test(momentum)]$statistic),
                  NSI = c(CS.reg.estimates[,t.test(nsi)]$estimate, CS.reg.estimates[, t.test(nsi)]$statistic),
                  ItA = c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic))




# Only Big Stocks
cross_reg <- factors %>% filter(pf.size == "Big")