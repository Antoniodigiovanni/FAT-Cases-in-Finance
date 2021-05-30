library(rstudioapi)

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

source("Factors.R")

# Temporary Model: BM_m, GPA, NOA, NSI, I/A, Size, Momentum, Beta

# Monthly cross-sectional regression (adding a country dummy + negative earning dummies)

cross_reg <- factors %>% 
  select(Id, ym, RET, Beta, BM_m, bm_m_dummy, 
         GPA, profits_dummy,
         NOA, noa_dummy, ItA,
         MV.USD.June, Momentum, country) %>% 
  drop_na(.)


cross_reg[BM_m<0]$BM_m <- 0
cross_reg[GPA<0]$GPA <- 0
cross_reg[NOA<0]$NOA <- 0

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[1],
                                  beta=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[2],
                                  bm_m=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[3],
                                  gpa=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[4],
                                  noa=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[5],
                                  ita=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[6],
                                  size=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[7],
                                  mom=lm(RET~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[8],
                                  no.obs=length(Id)),by=ym]


CS.reg.estimates<-CS.reg.estimates[order(ym)]


# Test with Returns in USD ---- Exact same results as RET (regarding the mean and the significance of the factors)
# CS.reg.estimates <- cross_reg[, .(intercept=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[1],
#                                   beta=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[2],
#                                   bm_m=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[3],
#                                   gpa=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[4],
#                                   noa=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[5],
#                                   ita=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[6],
#                                   size=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[7],
#                                   mom=lm(RET.USD~Beta+BM_m+GPA+NOA+ItA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy)$coefficient[8],
#                                   no.obs=length(Id)),by=ym]

Model_significance <- data.table(Beta = c(CS.reg.estimates[,t.test(beta)]$estimate, CS.reg.estimates[, t.test(beta)]$statistic),
                            BM_M = c(CS.reg.estimates[,t.test(bm_m)]$estimate, CS.reg.estimates[, t.test(bm_m)]$statistic),
                            GPA = c(CS.reg.estimates[,t.test(gpa)]$estimate, CS.reg.estimates[, t.test(gpa)]$statistic),
                            NOA = c(CS.reg.estimates[,t.test(noa)]$estimate, CS.reg.estimates[, t.test(noa)]$statistic),
                            ItA = c(CS.reg.estimates[,t.test(ita)]$estimate, CS.reg.estimates[, t.test(ita)]$statistic),
                            Size = c(CS.reg.estimates[,t.test(size)]$estimate, CS.reg.estimates[, t.test(size)]$statistic),
                            Mom = c(CS.reg.estimates[,t.test(mom)]$estimate, CS.reg.estimates[, t.test(mom)]$statistic))


# Rolling mean of the coefficients

# First possible method, longer, we would need to perform the rolling mean once for each column.

Avg_coef <- CS.reg.estimates %>%
  do(cbind(intercept = select(., intercept) %>%
             rollapplyr(list(seq(-36, -1)), mean, by.column = FALSE, fill = NA),
           date_col = select(., ym)))


# Second method, less rows and seems to provide the same results, check again.
Rolling_Avg <- rollapplyr(data=CS.reg.estimates[,-c("ym","no.obs")], list(seq(-36,-1)), mean, by.column=TRUE, fill=NA)
Rolling_Avg <- cbind(Rolling_Avg, CS.reg.estimates[,c("ym")])

# Use the rolling average to forecast the returns - multiply each coefficient by the current characteristics
Final_factors <- factors %>% select(Id,country,ym,Date,month,year,MV.USD,MV.USD.June,RET,RET.USD,Beta,BM_m,GPA,NOA,ItA,Momentum)#,NSI)

Final_factors <- left_join(Final_factors,
                           Rolling_Avg,#[,-c("intercept")], 
                           by=c("ym"))

Ret <- Final_factors %>%
  mutate(Exp.RET = (beta*Beta)+(BM_m*bm_m)+(GPA*gpa)+(NOA*noa)+(ItA*ita)+(MV.USD.June*size)+(Momentum*mom)) %>% 
  select(Id,country,ym,month,year,RET,RET.USD,Exp.RET) %>% as.data.table()


# Check Quintiles of returns and exp returns

Ret <- as.data.table(Ret)
  hlpvariable2 <- Ret[!is.na(RET)] %>% 
    group_by(country, ym) %>% 
    summarize(bb20 = quantile(RET, probs = c(0.2), na.rm = T),
              bb40 = quantile(RET, probs = c(0.4), na.rm = T),
              bb60 = quantile(RET, probs = c(0.6), na.rm = T),
              bb80 = quantile(RET, probs = c(0.8), na.rm = T)) %>% 
    select(ym, country, bb20, bb40, bb60, bb80)

  
Ret <- left_join(Ret, hlpvariable2,
                 by=c("ym","country"))

Ret[ , pf.ret := ifelse(RET>bb80, "Big",
                        ifelse((RET<=bb80 & RET>bb60),"LessBig",
                               ifelse((RET<=bb60 & RET>bb40),"Neutral",
                                      ifelse((RET<=bb40 & RET>bb20),"LessSmall",
                                             ifelse(RET<=bb20,"Small",NA)))))]
#Exp.Ret
Ret <- as.data.table(Ret)
hlpvariable2 <- Ret[!is.na(Exp.RET)] %>% 
  group_by(country, ym) %>% 
  summarize(bb20 = quantile(Exp.RET, probs = c(0.2), na.rm = T),
            bb40 = quantile(Exp.RET, probs = c(0.4), na.rm = T),
            bb60 = quantile(Exp.RET, probs = c(0.6), na.rm = T),
            bb80 = quantile(Exp.RET, probs = c(0.8), na.rm = T)) %>% 
  select(ym, country, bb20, bb40, bb60, bb80)

Ret <- Ret %>% select(-bb20, -bb40, -bb60, -bb80)
Ret <- left_join(Ret, hlpvariable2,
                 by=c("ym","country"))

Ret[ , pf.exp.ret := ifelse(Exp.RET>bb80, "Big",
                        ifelse((Exp.RET<=bb80 & Exp.RET>bb60),"LessBig",
                               ifelse((Exp.RET<=bb60 & Exp.RET>bb40),"Neutral",
                                      ifelse((Exp.RET<=bb40 & Exp.RET>bb20),"LessSmall",
                                             ifelse(Exp.RET<=bb20,"Small",NA)))))]
