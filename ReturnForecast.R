library(rstudioapi)

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

source("Factors.R")

# Temporary Model: BM_m, GPA, Size, Momentum

# Monthly cross-sectional regression (adding a country dummy + negative earning dummies)

cross_reg <- factors %>% 
  select(Id, ym, RET, BM_m, bm_m_dummy, 
         GPA, profits_dummy,
         LMV, Momentum, country) %>% 
  drop_na(.)


cross_reg[BM_m<=0]$BM_m <- 0
cross_reg[GPA<=0]$GPA <- 0
cross_reg <- cross_reg %>% mutate(BM_m = log(BM_m), GPA = log(GPA),
                                  MV.USD = log(MV.USD))

cross_reg[GPA == -Inf]$GPA <- 0
cross_reg[BM_m == -Inf]$BM_m <- 0
# cross_reg <- cross_reg %>% mutate(across(where(is.numeric), log))
#lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy ,data=cross_reg)

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[1],
                                  bm_m=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[2],
                                  gpa=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[3],
                                  size=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[4],
                                  mom=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[5],
                                  KOR=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[6],
                                  SGP=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[7],
                                  TWN=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[8],
                                  bm_m_dummy=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[9],
                                  profits_dummy=lm(RET~BM_m+GPA+MV.USD+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[10],
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

# Test expected returns on 5FM first
ffm_factors <- cross_reg %>% select(Id, country, ym, MV.USD.June, RET.USD, Beta, BM, OPBE, AG, op_dummy, bm_dummy)

ffm_factors <- left_join(ffm_factors,
                           Rolling_Avg,#[,-c("intercept")], 
                           by=c("ym"))

#check <- ffm_factors %>% filter(Id == "13039P") %>% drop_na()


Ret <- ffm_factors %>%
  mutate(Exp.RET = intercept + (beta*Beta)+(BM*bm)+(OPBE*opbe)+(AG*ag)+(MV.USD.June*size)+
           ifelse(country=="SGP", sgp, ifelse(country=="KOR", kor, ifelse(country=="TWN", twn, 0)))+
           op_dummy.x * op_dummy.y + bm_dummy.x * bm_dummy.y ) %>% 
  select(Id,country,ym,RET.USD,Exp.RET) %>% as.data.table()

# Use the rolling average to forecast the returns - multiply each coefficient by the current characteristics
Final_factors <- cross_reg %>% select(Id,country,ym,MV.USD.June,RET,Beta,BM_m,GPA,NOA,Momentum, bm_m_dummy,
                                      profits_dummy, noa_dummy)#,NSI)

Final_factors <- left_join(Final_factors,
                           Rolling_Avg,#[,-c("intercept")], 
                           by=c("ym"))

Ret <- Final_factors %>%
  mutate(Exp.RET = intercept+(beta*Beta)+(BM_m*bm_m)+(GPA*gpa)+(NOA*noa)+
           (MV.USD.June*size)+(Momentum*mom)+
           ifelse(country=="SGP", SGP, ifelse(country=="KOR", KOR, ifelse(country=="TWN", TWN, 0)))+
           bm_m_dummy.x*bm_m_dummy.y+profits_dummy.x*profits_dummy.y+noa_dummy.x*noa_dummy.y) %>% 
  select(Id,country,ym,RET,Exp.RET) %>% as.data.table()


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

# Check Yearly average of returns and expected returns
Ret[,year:=year(ym)]
quarter(Ret$ym)
Yearly_ret <- Ret %>% 
  mutate(RET = (RET/100+1)*100,
         Exp.RET = Exp.RET/100+1)*100 %>% 
  group_by(Id,year) %>%
  summarise(RET = prod(RET)-1,
            Exp.RET = prod(Exp.RET)-1)
  
  
  
# Run regression
lm(RET~Exp.RET+as.factor(country), data = Yearly_ret)
  