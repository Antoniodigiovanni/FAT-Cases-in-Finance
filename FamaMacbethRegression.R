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
# For some observations Beta = 0 because to calculate Beta we wanted to have at least 36 months of Data
# We drop those for the Fama MacBeth Regressions
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM, bm_dummy , OPBE, op_dummy , AG, MV.USD.June) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf" & Beta != 0)

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:11)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE and BM to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0


CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  no.obs=length(Id)),by=ym]


ffm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic))

# BM OPBE and AG are significant, all with expected Signs, Size and Beta insignificant
# Take the log of these variables to see if this changes the results
# Try the log of the variables
cross_reg_log <- cross_reg %>% mutate(BM = log(BM), OPBE = log(OPBE),
                                  MV.USD.June = log(MV.USD.June)) %>% replace(is.na(.), 0)

# There are some extraordinary observations for AG
plot <- cross_reg %>% arrange(desc(AG)) %>% slice(1:1000)
ggplot(plot, aes(x = AG, y = MV.USD.June)) +
  geom_point()

cross_reg_log <- cross_reg_log %>% filter(AG<1000)
# Set the variables that are -Inf to 0 again (taking the log caused that)
# Set negative values for OPBE and BM to 0
cross_reg_log[OPBE == "-Inf"]$OPBE <- 0
cross_reg_log[BM == "-Inf"]$BM <- 0

CS.reg.estimates <- cross_reg_log[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  no.obs=length(Id)),by=ym]


ffm_log <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic))


# Taking the log improved T Values for BM and slightly OPBE
# AG T values slightly worse
# No additional benefit to take the log

# Include Momentum to the model
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM, bm_dummy , OPBE, op_dummy,
                                AG, MV.USD.June, Momentum) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf" & Beta != 0)

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


sfm <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# When including Momentum OPBE is only a little bit above 2
# Everything else stays the same, Momentum not significant
# Lets try short term Momentum
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM, bm_dummy , OPBE, op_dummy,
                                AG, MV.USD.June, Momentum_short) %>% rename(Momentum = Momentum_short) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf" & Beta != 0)

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


sfm_short <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Here the momentum is even less significant
# Try to change the yearly BM with the monthly BM to see if this has an effect on Momentum
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM_m, bm_m_dummy , OPBE, op_dummy,
                                AG, MV.USD.June, Momentum) %>% 
  rename(BM = BM_m, bm_dummy = bm_m_dummy) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf" & Beta != 0)

# There are a lot of Inf this is because we lag this variable, exlude these
cross_reg <- cross_reg %>% filter(BM != "Inf" & BM != "-Inf")

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


sfm_monthly <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# BM is now even more significant, also Momentum has a much higher T Value but only 1.8
# Significance of OPBE drops

# Lets try short term Momentum with the monthly BM
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM_m, bm_m_dummy , OPBE, op_dummy,
                                AG, MV.USD.June, Momentum_short) %>% 
  rename(Momentum = Momentum_short, BM = BM_m, bm_dummy = bm_m_dummy) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf" & Beta != 0)

# There are a lot of Inf this is because we lag this variable, exlude these
cross_reg <- cross_reg %>% filter(BM != "Inf" & BM != "-Inf")

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM<0]$BM <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[2],
                                  bm=lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[7],
                                  kor = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[8],
                                  sgp = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[9],
                                  twn = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[10],
                                  bm_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[11],
                                  op_dummy = lm(RET~Beta+BM+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+op_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


sfm_short_monthly <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                        BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                        OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                        AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                        Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                        Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Interestingly Momentum again less significant and BM, OPBE and AG significant

# Comparison Value Variables
cross_reg <- factors %>% select(Id, ym, country, RET.USD, Beta, BM, bm_dummy, BM_m, bm_m_dummy, OPBE, op_dummy,
                                AG, MV.USD.June, Momentum) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf")

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_bm=mean(BM), mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_bmm=mean(BM_m), mean_size=mean(MV.USD.June))

cut_bm_top <- quantile(cross_reg$mean_bm, 0.99)
cut_bm_bottom <- quantile(cross_reg$mean_bm, 0.01)

cut_bmm_top <- quantile(cross_reg$mean_bmm, 0.99)
cut_bmm_bottom <- quantile(cross_reg$mean_bmm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_bm_top = (mean_bm >= cut_bm_top), 
         outlier_bm_bottom = mean_bm <= cut_bm_bottom,
         outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_bmm_top = (mean_bmm >= cut_bmm_top), 
         outlier_bmm_bottom = mean_bmm <= cut_bmm_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_bm_top & ! outlier_bm_bottom & 
           !outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_bmm_top & ! outlier_bmm_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:14)
cross_reg <- data.table(cross_reg)

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
                                  kor = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[9],
                                  sgp = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[10],
                                  twn = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[11],
                                  bm_dummy = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[12],
                                  bm_m_dummy = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[13],
                                  op_dummy = lm(RET.USD~Beta+BM+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_dummy+bm_m_dummy+op_dummy)$coefficient[14],
                                  no.obs=length(Id)),by=ym]


value1 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                  BM = c(CS.reg.estimates[,t.test(bm)]$estimate, CS.reg.estimates[, t.test(bm)]$statistic),
                  BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                  OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                  AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                  Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                  Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Drop yearly BM
cross_reg <- factors %>% select(Id, ym, country, RET.USD, Beta, BM_m, bm_m_dummy, OPBE, op_dummy,
                                AG, MV.USD.June, Momentum) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf")

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_bmm=mean(BM_m), mean_size=mean(MV.USD.June))

cut_bmm_top <- quantile(cross_reg$mean_bmm, 0.99)
cut_bmm_bottom <- quantile(cross_reg$mean_bmm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_bmm_top = (mean_bmm >= cut_bmm_top), 
         outlier_bmm_bottom = mean_bmm <= cut_bmm_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_bmm_top & ! outlier_bmm_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[3],
                                  opbe = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[4],
                                  ag = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[5],
                                  size = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[6],
                                  mom = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[7],
                                  kor = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[8],
                                  sgp = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[9],
                                  twn = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[10],
                                  bm_m_dummy = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[11],
                                  op_dummy = lm(RET.USD~Beta+BM_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


value2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# Add more Value Variables to see if some perform better
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM_m, bm_m_dummy, EP_m, earnings_m_dummy,
                                CP_m, cashflow_m_dummy, OPBE, op_dummy,
                                AG, MV.USD.June, Momentum) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf" & OPBE != "Inf" & OPBE != "-Inf")

# Removes all the Inf observations
cross_reg <- cross_reg %>% filter(across(everything(), ~ !is.infinite(.x)))

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_Beta=mean(Beta), mean_opbe=mean(OPBE), mean_ag=mean(AG),
         mean_mom=mean(Momentum), mean_bmm=mean(BM_m), mean_size=mean(MV.USD.June),
         mean_cp=mean(CP_m), mean_ep=mean(EP_m))

cut_bmm_top <- quantile(cross_reg$mean_bmm, 0.99)
cut_bmm_bottom <- quantile(cross_reg$mean_bmm, 0.01)

cut_ep_top <- quantile(cross_reg$mean_ep, 0.99)
cut_ep_bottom <- quantile(cross_reg$mean_ep, 0.01)

cut_cp_top <- quantile(cross_reg$mean_cp, 0.99)
cut_cp_bottom <- quantile(cross_reg$mean_cp, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_opbe_top <- quantile(cross_reg$mean_opbe, 0.99)
cut_opbe_bottom <- quantile(cross_reg$mean_opbe, 0.01)

cut_ag_top <- quantile(cross_reg$mean_ag, 0.99)
cut_ag_bottom <- quantile(cross_reg$mean_ag, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_opbe_top = (mean_opbe >= cut_opbe_top), 
         outlier_opbe_bottom = mean_opbe <= cut_opbe_bottom,
         outlier_ag_top = (mean_ag >= cut_ag_top), 
         outlier_ag_bottom = mean_ag <= cut_ag_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_bmm_top = (mean_bmm >= cut_bmm_top), 
         outlier_bmm_bottom = mean_bmm <= cut_bmm_bottom,
         outlier_ep_top = (mean_ep >= cut_ep_top), 
         outlier_ep_bottom = mean_ep <= cut_ep_bottom,
         outlier_cp_top = (mean_cp >= cut_cp_top), 
         outlier_cp_bottom = mean_cp <= cut_cp_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom) %>% 
  filter(!outlier_beta_top & ! outlier_beta_bottom &
           !outlier_opbe_top & ! outlier_opbe_bottom &
           !outlier_ag_top & ! outlier_ag_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_bmm_top & ! outlier_bmm_bottom &
           !outlier_size_top & ! outlier_size_bottom &
           !outlier_cp_top & ! outlier_cp_bottom &
           !outlier_ep_top & ! outlier_ep_bottom)

cross_reg <- cross_reg %>% select(1:16)
cross_reg <- data.table(cross_reg)

# Set negative values for OPBE to 0
cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[CP_m<0]$CP_m <- 0
cross_reg[EP_m<0]$EP_m <- 0


CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[2],
                                  bm_m=lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[3],
                                  ep_m = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[4],
                                  cp_m = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[5],
                                  opbe = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[6],
                                  ag = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[7],
                                  size =lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[8],
                                  mom = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[9],
                                  kor = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[10],
                                  sgp = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[11],
                                  twn = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[12],
                                  bm_m_dummy = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[13],
                                  earnings_dummy = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[14],
                                  cashflow_dummy = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[15],
                                  op_dummy = lm(RET~Beta+BM_m+EP_m+CP_m+OPBE+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+earnings_m_dummy+cashflow_m_dummy+op_dummy)$coefficient[16],
                                  no.obs=length(Id)),by=ym]


value2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     EP_M = c(CS.reg.estimates[,t.test(ep_m)]$estimate, CS.reg.estimates[, t.test(ep_m)]$statistic),
                     CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Other monthly variables are not significant so we go with monthly BM as value variable from here

# Comparison Profitability Variables
# We dont include here some of the profitability variables because we had high correlation between some of them
cross_reg <- factors %>% 
  select(Id, ym, RET, Beta, BM_m, bm_m_dummy,ROE, ROA, GPA, profits_dummy, OPBE, op_dummy,
         NOA, noa_dummy, AG, MV.USD.June, Momentum, country) %>% 
  drop_na(.) %>% 
  filter(AG != "Inf")

# Removes all the Inf observations
cross_reg <- cross_reg %>% filter(across(everything(), ~ !is.infinite(.x)))

cross_reg[OPBE<0]$OPBE <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[NOA<0]$NOA <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  roe=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  roa=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  gpa=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  opbe=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  noa=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  ag=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  size=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  mom=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[11],
                                  kor=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[12],
                                  sgp=lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[13],
                                  twn = lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[14],
                                  bm_dummy = lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[15],
                                  op_dummy = lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[16],
                                  prof_dummy = lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[17],
                                  noa_dummy = lm(RET~Beta+BM_m+ROE+ROA+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+op_dummy+profits_dummy+noa_dummy)$coefficient[18],
                                  no.obs=length(Id)),by=ym]

profitability <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     ROE = c(CS.reg.estimates[,t.test(roe)]$estimate, CS.reg.estimates[, t.test(roe)]$statistic),
                     ROA = c(CS.reg.estimates[,t.test(roa)]$estimate, CS.reg.estimates[, t.test(roa)]$statistic),
                     GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                     OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                     NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                     AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# BM NOA highly significant, Mom close
# Drop ROE, ROA to see if GPA and OPBE become significant again

# Drop ROE, ROA and OA to check if OL is still insignificant since it had high tstat in the sorts
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[3],
                                  gpa=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[4],
                                  opbe=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[5],
                                  noa=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[6],
                                  ag=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[7],
                                  size=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[8],
                                  mom=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[9],
                                  kor=lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[10],
                                  sgp =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  twn =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  bm_dummy =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  profits_dummy =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  op_dummy =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  noa_dummy =lm(RET~Beta+BM_m+GPA+OPBE+NOA+AG+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+op_dummy+noa_dummy)$coefficient[11],
                                  no.obs=length(Id)),by=ym]

profitability2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                            BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                            GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                            OPBE = c(CS.reg.estimates[,t.test(opbe)]$estimate, CS.reg.estimates[, t.test(opbe)]$statistic),
                            NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                            AG = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                            Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                            Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))

# GPA is not significant, drop OPBE
# Keep GPA and NOA 

# Investment variables
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


# Strongest Factors
cross_reg <- factors %>% select(Id, ym, country, RET.USD, Beta, BM_m, bm_m_dummy,
                                GPA, profits_dummy, NOA, MV.USD.June, Momentum) %>% 
  drop_na(.) %>% 
  filter(GPA != "Inf" & GPA != "-Inf" & BM_m != "-Inf" & BM_m != "Inf" & NOA != "Inf" & NOA != "-Inf")

# Winsorize all explanatory variables at the 1st and 99th percentile
cross_reg <- cross_reg %>%  
  group_by(Id) %>%  
  mutate(mean_Beta=mean(Beta), mean_gpa=mean(GPA), mean_mom=mean(Momentum),
         mean_bmm=mean(BM_m), mean_size=mean(MV.USD.June), mean_noa=mean(NOA))

cut_bmm_top <- quantile(cross_reg$mean_bmm, 0.99)
cut_bmm_bottom <- quantile(cross_reg$mean_bmm, 0.01)

cut_beta_top <- quantile(cross_reg$mean_Beta, 0.99)
cut_beta_bottom <- quantile(cross_reg$mean_Beta, 0.01)

cut_gpa_top <- quantile(cross_reg$mean_gpa, 0.99)
cut_gpa_bottom <- quantile(cross_reg$mean_gpa, 0.01)

cut_size_top <- quantile(cross_reg$mean_size, 0.99)
cut_size_bottom <- quantile(cross_reg$mean_size, 0.01)

cut_mom_top <- quantile(cross_reg$mean_mom, 0.99)
cut_mom_bottom <- quantile(cross_reg$mean_mom, 0.01)

cut_noa_top <- quantile(cross_reg$mean_noa, 0.99)
cut_noa_bottom <- quantile(cross_reg$mean_noa, 0.01)

cross_reg <- cross_reg %>% 
  group_by(Id) %>%  
  mutate(outlier_beta_top = (mean_Beta >= cut_beta_top), 
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_gpa_top = (mean_gpa >= cut_gpa_top), 
         outlier_gpa_bottom = mean_gpa <= cut_gpa_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top), 
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_bmm_top = (mean_bmm >= cut_bmm_top), 
         outlier_bmm_bottom = mean_bmm <= cut_bmm_bottom,
         outlier_size_top = (mean_size >= cut_size_top), 
         outlier_size_bottom = mean_size <= cut_size_bottom,
         outlier_noa_top = (mean_noa >= cut_noa_top), 
         outlier_noa_bottom = mean_noa <= cut_noa_bottom) %>% 
  filter(!outlier_beta_top & ! outlier_beta_bottom &
           !outlier_gpa_top & ! outlier_gpa_bottom &
           !outlier_noa_top & ! outlier_noa_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_bmm_top & ! outlier_bmm_bottom &
           !outlier_size_top & ! outlier_size_bottom)

cross_reg <- cross_reg %>% select(1:12)
cross_reg <- data.table(cross_reg)

# Set negative values for GPA and BM_m to 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[BM_m<0]$BM_m <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[3],
                                  gpa = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[4],
                                  ag = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[5],
                                  size = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[6],
                                  mom = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[7],
                                  kor = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[8],
                                  sgp = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[9],
                                  twn = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[10],
                                  bm_m_dummy = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[11],
                                  profits_dummy = lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy)$coefficient[12],
                                  no.obs=length(Id)),by=ym]


strongest_factor <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                     BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                     GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                     NOA = c(CS.reg.estimates[,t.test(ag)]$estimate, CS.reg.estimates[, t.test(ag)]$statistic),
                     Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                     Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


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




## Only Big Stocks
cross_reg <- factors %>% filter(pf.size == "Big") %>%
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

#Strongest vs 5-Factor

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
strongVSffmBig <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
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

# strong big
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

strongBig <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                   BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                   CP_M = c(CS.reg.estimates[,t.test(cp_m)]$estimate, CS.reg.estimates[, t.test(cp_m)]$statistic),
                   GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                   NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                   Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                   Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic),
                   NSI = c(CS.reg.estimates[,t.test(nsi)]$estimate, CS.reg.estimates[, t.test(nsi)]$statistic),
                   ItA= c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic))

# strong big, drop NSI,  CP m
CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  gpa=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  noa=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  size=lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  mom =lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  ita =lm(RET.USD~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+ItA+as.factor(country)+bm_m_dummy+cashflow_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  no.obs=length(Id)),by=ym]

strongBig2 <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                        BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                        GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                        NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                        Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                        Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic),
                        ItA= c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic))
