library(rstudioapi)

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

source("Factors.R")
#source("FamaMacBethRegression.R")


#table(small_static$INDM)


# Strongest Factors = BM_m, GPA, NOA, Mom, we keep Beta and Size for control
cross_reg <- factors %>% select(Id, ym, country, RET, Beta, BM_m, bm_m_dummy,
                                GPA, profits_dummy, NOA, noa_dummy, MV.USD.June, Momentum) %>% 
  drop_na(.)

# Removes all the Inf observations
cross_reg <- cross_reg %>% filter(across(everything(), ~ !is.infinite(.x)))

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

cross_reg <- cross_reg %>% select(1:13)
cross_reg <- data.table(cross_reg)

# Set negative values for GPA and BM_m to 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[BM_m<0]$BM_m <- 0
cross_reg[NOA<0]$NOA <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  gpa =lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  noa =lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  size=lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  mom = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  kor = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  sgp = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[9],
                                  twn = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[10],
                                  bm_m_dummy = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[11],
                                  profits_dummy = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[12],
                                  noa_dummy = lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[13],
                                  no.obs=length(Id)),by=ym]


strongest_factor <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                               BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                               GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                               NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                               Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                               Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))



CS.reg.estimates <- CS.reg.estimates[order(ym)]


# Rolling mean of the coefficients

# First possible method, longer, we would need to perform the rolling mean once for each column.
# 
# Avg_coef <- CS.reg.estimates %>%
#   do(cbind(intercept = select(., intercept) %>%
#              rollapplyr(list(seq(-36, -1)), mean, by.column = FALSE, fill = NA),
#            date_col = select(., ym)))


# Second method, less rows and seems to provide the same results, check again.
Rolling_Avg <- rollapplyr(data=CS.reg.estimates[,-c("ym","no.obs")], list(seq(-36,-1)), mean, by.column=TRUE, fill=NA)
Rolling_Avg <- cbind(Rolling_Avg, CS.reg.estimates[,c("ym")])

# Test expected returns on 5FM first
# ffm_factors <- cross_reg %>% select(Id, country, ym, MV.USD.June, RET.USD, Beta, BM, OPBE, AG, op_dummy, bm_dummy)
# 
# ffm_factors <- left_join(ffm_factors,
#                            Rolling_Avg,#[,-c("intercept")], 
#                            by=c("ym"))
# 
# ffm_factors <- ffm_factors %>% mutate(SGP = ifelse(country == "SGP", 1, 0))
# ffm_factors <- ffm_factors %>% mutate(KOR = ifelse(country == "KOR", 1, 0))
# ffm_factors <- ffm_factors %>% mutate(TWN = ifelse(country == "TWN", 1, 0))


#check <- ffm_factors %>% filter(Id == "13039P") %>% drop_na()


# Ret <- ffm_factors %>%
#   mutate(Exp.RET = intercept + (beta*Beta)+(BM*bm)+(OPBE*opbe)+(AG*ag)+(MV.USD.June*size)+
#            SGP*sgp + KOR*kor + TWN*twn + 
#            op_dummy.x * op_dummy.y + bm_dummy.x * bm_dummy.y ) %>% 
#   select(Id,country,ym,RET.USD,Exp.RET) %>% as.data.table()


# Use the rolling average to forecast the returns - multiply each coefficient by the current characteristics
Final_factors <- factors %>% select(Id,country,ym,RET, Beta, BM_m, GPA, NOA, Momentum, bm_m_dummy,
                                      profits_dummy, noa_dummy) %>% drop_na()

Final_factors <- left_join(Final_factors,
                           Rolling_Avg,#[,-c("intercept")], 
                           by=c("ym"))

Final_factors <- Final_factors %>% mutate(SGP = ifelse(country == "SGP", 1, 0))
Final_factors <- Final_factors %>% mutate(KOR = ifelse(country == "KOR", 1, 0))
Final_factors <- Final_factors %>% mutate(TWN = ifelse(country == "TWN", 1, 0))
Final_factors <- Final_factors %>% mutate(profits_dummy.y = ifelse(is.na(profits_dummy.y), 0, profits_dummy.y))
Final_factors <- Final_factors %>% mutate(bm_m_dummy.y = ifelse(is.na(bm_m_dummy.y), 0, bm_m_dummy.y))
Final_factors <- Final_factors %>% mutate(noa_dummy.y = ifelse(is.na(noa_dummy.y), 0, noa_dummy.y))

# Removes all the Inf observations
Final_factors <- Final_factors %>% filter(across(everything(), ~ !is.infinite(.x)))

# Remove Beta and Momentum = 0 because these are stocks without long history
Final_factors <- Final_factors %>% filter(Beta != 0 & Momentum != 0)


#Winsorize the data before calculating return

Final_factors <- Final_factors %>%
  group_by(Id) %>%
  mutate(mean_Beta=mean(Beta), mean_gpa=mean(GPA), mean_mom=mean(Momentum),
         mean_bmm=mean(BM_m), mean_noa=mean(NOA))

cut_bmm_top <- quantile(Final_factors$mean_bmm, 0.99)
cut_bmm_bottom <- quantile(Final_factors$mean_bmm, 0.01)

cut_beta_top <- quantile(Final_factors$mean_Beta, 0.99)
cut_beta_bottom <- quantile(Final_factors$mean_Beta, 0.01)

cut_gpa_top <- quantile(Final_factors$mean_gpa, 0.99)
cut_gpa_bottom <- quantile(Final_factors$mean_gpa, 0.01)

cut_mom_top <- quantile(Final_factors$mean_mom, 0.99)
cut_mom_bottom <- quantile(Final_factors$mean_mom, 0.01)

cut_noa_top <- quantile(Final_factors$mean_noa, 0.99)
cut_noa_bottom <- quantile(Final_factors$mean_noa, 0.01)

Final_factors <- Final_factors %>%
  group_by(Id) %>%
  mutate(outlier_beta_top = (mean_Beta >= cut_beta_top),
         outlier_beta_bottom = mean_Beta <= cut_beta_bottom,
         outlier_gpa_top = (mean_gpa >= cut_gpa_top),
         outlier_gpa_bottom = mean_gpa <= cut_gpa_bottom,
         outlier_mom_top = (mean_mom >= cut_mom_top),
         outlier_mom_bottom = mean_mom <= cut_mom_bottom,
         outlier_bmm_top = (mean_bmm >= cut_bmm_top),
         outlier_bmm_bottom = mean_bmm <= cut_bmm_bottom,
         outlier_noa_top = (mean_noa >= cut_noa_top),
         outlier_noa_bottom = mean_noa <= cut_noa_bottom) %>%
  filter(!outlier_beta_top & ! outlier_beta_bottom &
           !outlier_gpa_top & ! outlier_gpa_bottom &
           !outlier_noa_top & ! outlier_noa_bottom &
           !outlier_mom_top & ! outlier_mom_bottom &
           !outlier_bmm_top & ! outlier_bmm_bottom )

#Final_factors <- Final_factors %>% select(1:12)
#Final_factors <- data.table(Final_factors)


Ret <- Final_factors %>%
  mutate(Exp.RET = intercept+(BM_m*bm_m)+(GPA*gpa)+
           (Momentum*mom) + NOA*noa+ sgp*SGP + kor*KOR + twn*TWN +
           bm_m_dummy.x*bm_m_dummy.y+profits_dummy.x*profits_dummy.y+
           noa_dummy.x*noa_dummy.y) %>% 
  select(Id,country,ym,RET,Exp.RET) %>% as.data.table()

Ret <- Ret %>% drop_na()

# Run regression
lm(RET~Exp.RET+as.factor(country), data = Ret)
