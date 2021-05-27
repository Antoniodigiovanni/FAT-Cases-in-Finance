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
# First test the traditional Fama/Macbeth 5 Factors

cross_reg <- factors %>% select(Id, ym, RET.USD, Beta, BM, bm_dummy , OPBE, op_dummy , AG, MV.USD.June, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

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

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  no.obs=length(Id)),by=ym]


sfm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Comparison Value Variables
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM, BM_m, bm_dummy, OPBE, op_dummy, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  bm_m=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  opbe = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  ag = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  size = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  mom = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  no.obs=length(Id)),by=ym]


value1 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Need monthly book to market dummy variable
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, EP_m, earnings_dummy, CP_m, cashflow_dummy,
         OPBE, op_dummy, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf") %>% mutate(bm_dummy = ifelse(BM_m < 0, 1, 0))

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[EP_m<0]$EP_m <- 0
cross_reg[CP_m<0]$CP_m <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[3],
                                  ep_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[4],
                                  cp_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[5],
                                  opbe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[6],
                                  ag=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[7],
                                  size=lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[8],
                                  mom = lm(RET.USD~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy)$coefficient[9],
                                  no.obs=length(Id)),by=ym]


value2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                     CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Comparison Profitability Variables
# Can OA be negative?
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, EP_m, earnings_dummy,
         CP_m, cashflow_dummy, GPA, profits_dummy, OPBE, op_dummy, NOA, OA, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf") %>% mutate(bm_dummy = ifelse(BM_m < 0, 1, 0))

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[EP_m<0]$EP_m <- 0
cross_reg[CP_m<0]$CP_m <- 0
cross_reg[GPA<0]$GPA <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[3],
                                  ep_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[4],
                                  cp_m=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[5],
                                  gpa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[6],
                                  opbe=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[7],
                                  noa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[8],
                                  oa=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[9],
                                  ag=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[10],
                                  size=lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[11],
                                  mom = lm(RET.USD~Beta+BM_m+EP_m+CP_m+GPA+OPBE+NOA+OA+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy+earnings_dummy+cashflow_dummy+profits_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]

profitability <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                     CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                     GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                     OA = c(CS.reg.estimates[,t.test(oa)]$estimate, CS.reg.estimates[, t.test(oa)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))



# Comparison Investment Variables


# Strongest EM Variables




# Only Big Stocks
cross_reg <- factors %>% filter(pf.size == "Big")