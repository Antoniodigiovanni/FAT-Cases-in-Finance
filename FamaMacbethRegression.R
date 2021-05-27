library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(broom)

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

# Include for dummy for negative earnings, cash flow and gross profits 
# First test the traditional Fama/Macbeth 5 Factors

cross_reg <- factors %>% select(Id, ym, RET.USD, Beta, BM, OPBE, AG, MV.USD.June, country) %>% drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[1],
                                    gamma=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[2],
                                    gamma.value=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[3],
                                    gamma.profitability = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[4],
                                    gamma.investment = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[5],
                                    gamma.size = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country))$coefficient[6],
                                        no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.value)]
CS.reg.estimtates[,t.test(gamma.profitability)]
CS.reg.estimtates[,t.test(gamma.investment)]
CS.reg.estimtates[,t.test(gamma.size)]

# Include Momentum to the model
cross_reg <- factors %>% select(Id, ym, RET.USD, Beta, BM, OPBE, AG, MV.USD.June, Momentum, country) %>% drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[1],
                                   gamma=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[2],
                                   gamma.value=lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[3],
                                   gamma.profitability = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[4],
                                   gamma.investment = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[5],
                                   gamma.size = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[6],
                                   gamma.Mom = lm(RET.USD~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[7],
                                   no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.value)]
CS.reg.estimtates[,t.test(gamma.profitability)]
CS.reg.estimtates[,t.test(gamma.investment)]
CS.reg.estimtates[,t.test(gamma.size)]
CS.reg.estimtates[,t.test(gamma.Mom)]


# Comparison Value Variables
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM, BM_m, OPBE, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[1],
                                   gamma=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[2],
                                   gamma.value=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[3],
                                   gamma.value_monthly=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[4],
                                   gamma.profitability = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[5],
                                   gamma.investment = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[6],
                                   gamma.size = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[7],
                                   gamma.Mom = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[8],
                                   no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.value)]
CS.reg.estimtates[,t.test(gamma.value_monthly)]
CS.reg.estimtates[,t.test(gamma.profitability)]
CS.reg.estimtates[,t.test(gamma.investment)]
CS.reg.estimtates[,t.test(gamma.size)]
CS.reg.estimtates[,t.test(gamma.Mom)]

cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM, BM_m, EP_m, CP_m, OPBE, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[1],
                                   gamma=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[2],
                                   gamma.bm_m=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[3],
                                   gamma.ep_m=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[4],
                                   gamma.cp_m=lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[5],
                                   gamma.profitability = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[6],
                                   gamma.investment = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[7],
                                   gamma.size = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[8],
                                   gamma.Mom = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[9],
                                   no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.bm_m)]
CS.reg.estimtates[,t.test(gamma.ep_m)]
CS.reg.estimtates[,t.test(gamma.cp_m)]
CS.reg.estimtates[,t.test(gamma.profitability)]
CS.reg.estimtates[,t.test(gamma.investment)]
CS.reg.estimtates[,t.test(gamma.size)]
CS.reg.estimtates[,t.test(gamma.Mom)]

# Comparison Profitability Variables
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, EP_m, CP_m, GPA, OPBE, NOA, OA, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[1],
                                   gamma=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[2],
                                   gamma.bm_m=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[3],
                                   gamma.ep_m=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[4],
                                   gamma.cp_m=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[5],
                                   gamma.gpa = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[6],
                                   gamma.opbe = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[7],
                                   gamma.noa = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[8],
                                   gamma.oa = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[9],
                                   gamma.investment = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[10],
                                   gamma.size = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[11],
                                   gamma.Mom = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[12],
                                   no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.bm_m)]
CS.reg.estimtates[,t.test(gamma.ep_m)]
CS.reg.estimtates[,t.test(gamma.cp_m)]
CS.reg.estimtates[,t.test(gamma.gpa)]
CS.reg.estimtates[,t.test(gamma.opbe)]
CS.reg.estimtates[,t.test(gamma.noa)]
CS.reg.estimtates[,t.test(gamma.oa)]
CS.reg.estimtates[,t.test(gamma.investment)]
CS.reg.estimtates[,t.test(gamma.size)]
CS.reg.estimtates[,t.test(gamma.mom)]

# Comparison Investment Variables
cross_reg <- factors %>% 
  select(Id, ym, RET.USD, Beta, BM_m, EP_m, CP_m, GPA, NOA, AG, ItA, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")
CS.reg.estimtates <- cross_reg[, .(gamma_zero=lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[1],
                                   gamma=lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[2],
                                   gamma.bm_m=lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[3],
                                   gamma.ep_m=lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[4],
                                   gamma.cp_m=lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[5],
                                   gamma.gpa = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[6],
                                   gamma.noa = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[7],
                                   gamma.oa = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[8],
                                   gamma.ag = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[9],
                                   gamma.ItA = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[10],
                                   gamma.size = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[11],
                                   gamma.Mom = lm(RET.USD~Beta+BM_m+AG+MV.USD.June+Momentum+as.factor(country))$coefficient[12],
                                   no.obs=length(Id)),by=ym]


CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]
CS.reg.estimtates[,t.test(gamma.bm_m)]
CS.reg.estimtates[,t.test(gamma.ep_m)]
CS.reg.estimtates[,t.test(gamma.cp_m)]
CS.reg.estimtates[,t.test(gamma.gpa)]
CS.reg.estimtates[,t.test(gamma.noa)]
CS.reg.estimtates[,t.test(gamma.ag)]
CS.reg.estimtates[,t.test(gamma.ItA)]
CS.reg.estimtates[,t.test(gamma.size)]
CS.reg.estimtates[,t.test(gamma.mom)]

# Strongest EM Variables




# Only Big Stocks
cross_reg <- factors %>% filter(pf.size == "Big")