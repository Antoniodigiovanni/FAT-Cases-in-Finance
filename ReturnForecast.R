library(rstudioapi)

# Set the working directory to the script directory
setwd(dirname(getActiveDocumentContext()$path)) 

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

source("Factors.R")

#Data from Datastream
load("FAT_monthly.RData")
load("FAT_static.RData")
# Yearly Accounting Data from Worldscope
load("FAT_yearly.RData")

# Get data from year 1994 to ensure data quality
FAT.monthly[, month := month(Date)]
FAT.monthly[, year := year(Date)]
FAT.monthly <- FAT.monthly %>% filter(year > 1993)

small_static <- FAT.static %>% select(Id, INDM)

all_data <- merge(FAT.monthly, small_static, by = "Id")
#table(small_static$INDM)

# Banks, Consumer Finance, FInancial Admin., Insurance Brokers, Mortgage Finance, Investment Services,
# Specialty Finance, Venture Capital Trust, Private Equity, Real Estate Hold, Dev, Reinsurance,
# Life Insurance, Asset Managers
# Exclude financial companies
cols_exlude <- c("Banks", "Consumer Finance", "Financial Admin.", "Insurance Brokers", "Mortgage Finance",
                 "Investment Services", "Specialty Finance", "Venture Capital Trust", "Private Equity",
                 "Real Estate Hold, Dev", "Reinsurance", "Life Insurance", "Asset Managers")

all_data <- all_data %>% filter(!(INDM %in% cols_exlude)) %>% select(-INDM)

# Following Hou et al.(2018) and excluding all micro stocks
# Create MV.USD.June to get yearly MV that matches with yearly accounting data
# MV.USD.June in Year y is for July year y until June y + 1
hlpvariable <- all_data %>% group_by(Id, year) %>% filter(month == 6)
hlpvariable <- hlpvariable %>% select(Id, year, MV.USD, MV) %>% rename(MV.USD.June = MV.USD, MV.June = MV)
all_data[,hcjun := ifelse(month>=7,year,year-1)]
all_data <- merge(all_data, hlpvariable, by.x = c("Id", "hcjun"), by.y = c("Id", "year"),
                  all.x = T)

# Define stocks into three size groups for each country
setorder(all_data, country, Date, -MV.USD.June)
hlpvariable <- all_data[month == 7 & !is.na(MV.USD.June)] %>% group_by(country, year) %>% 
  mutate(pf.size = ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.97,"Micro",
                          ifelse((cumsum(MV.USD.June)/sum(MV.USD.June))>=0.90,"Small","Big"))) %>% 
  select(country, year, pf.size, Id)

all_data <- merge(all_data,hlpvariable,
                  by.x=c("hcjun","Id", "country"),
                  by.y=c("year","Id", "country"),
                  all.x=T)

# Percentage of Micro Stocks in our sample
# How many unique micro stocks are in our sample
micro_stocks <- all_data %>% filter(pf.size == "Micro")
nrow(all_data %>% filter(pf.size == "Micro")) / nrow(all_data)

all_data <- all_data %>% filter(pf.size != "Micro")

all_data <- merge(all_data, FAT.yearly, by.x = c("Id", "year"), by.y = c("Id", "YEAR"), all.x = T)

current_factors <- all_data %>% mutate(
  BM_m = (WC03501+ifelse(is.na(WC03263),0,WC03263)) / (MV*1000),
  bm_m_dummy = ifelse(BM_m < 0, 1, 0),
  GPA =  (WC01001 - WC01501) / WC02999,
  profits_dummy = ifelse(GPA < 0, 1, 0)
) %>% 
  select(Id, country.x, Date, month, year, MV, MV.USD.June, RET.USD, RET, ym,
         BM_m, bm_m_dummy, GPA, profits_dummy,
         pf.size, hcjun) %>% 
  rename(country = country.x) %>% drop_na(MV.USD.June)

# Include FFtF and we use EBITDA - EBIT to get depreciation amount

#Momentum
Momentum <- all_data %>% group_by(Id) %>% mutate(RET.adj = RET/100 + 1) %>% 
  do(cbind(reg_col = select(., RET.adj) %>% 
             rollapplyr(list(seq(-12, -2)), prod, by.column = FALSE, fill = NA),
           date_col = select(., Date))) %>% 
  ungroup() %>% rename("Momentum" = reg_col)

Momentum <- Momentum %>% mutate(Momentum = (Momentum-1)*100)

current_factors <- left_join(current_factors,
                     Momentum,
                     by=c("Id", "Date"))

# Not Sure if Momentum is correct

# Temporary Model: BM_m, GPA, Size, Momentum

# Monthly cross-sectional regression (adding a country dummy + negative earning dummies)

cross_reg <- factors %>% 
  select(Id, ym, RET, BM_m, bm_m_dummy, 
         GPA, profits_dummy,
         LMV, Momentum, country) %>% 
  drop_na(.)


cross_reg[BM_m<=0]$BM_m <- 0
cross_reg[GPA<=0]$GPA <- 0
#cross_reg <- cross_reg %>% mutate(BM_m = log(BM_m), GPA = log(GPA),
                                  #LMV = log(LMV))

#cross_reg[GPA == -Inf]$GPA <- 0
#cross_reg[BM_m == -Inf]$BM_m <- 0
# cross_reg <- cross_reg %>% mutate(across(where(is.numeric), log))
#lm(RET~Beta+BM_m+GPA+NOA+MV.USD.June+Momentum+as.factor(country)+bm_m_dummy+profits_dummy+noa_dummy ,data=cross_reg)

CS.reg.estimates <- cross_reg[, .(intercept=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[1],
                                  bm_m=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[2],
                                  gpa=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[3],
                                  size=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[4],
                                  mom=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[5],
                                  KOR=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[6],
                                  SGP=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[7],
                                  TWN=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[8],
                                  bm_m_dummy=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[9],
                                  profits_dummy=lm(RET~BM_m+GPA+LMV+Momentum+as.factor(country)+bm_m_dummy+profits_dummy, na.action = na.omit)$coefficient[10],
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

ffm_factors <- ffm_factors %>% mutate(SGP = ifelse(country == "SGP", 1, 0))
ffm_factors <- ffm_factors %>% mutate(KOR = ifelse(country == "KOR", 1, 0))
ffm_factors <- ffm_factors %>% mutate(TWN = ifelse(country == "TWN", 1, 0))


#check <- ffm_factors %>% filter(Id == "13039P") %>% drop_na()


Ret <- ffm_factors %>%
  mutate(Exp.RET = intercept + (beta*Beta)+(BM*bm)+(OPBE*opbe)+(AG*ag)+(MV.USD.June*size)+
           SGP*sgp + KOR*kor + TWN*twn + 
           op_dummy.x * op_dummy.y + bm_dummy.x * bm_dummy.y ) %>% 
  select(Id,country,ym,RET.USD,Exp.RET) %>% as.data.table()


# Use the rolling average to forecast the returns - multiply each coefficient by the current characteristics
Final_factors <- factors %>% select(Id,country,ym,RET.USD, Beta, BM_m, GPA, NOA, Momentum, bm_m_dummy,
                                      profits_dummy) %>% drop_na()

Final_factors <- left_join(Final_factors,
                           Rolling_Avg,#[,-c("intercept")], 
                           by=c("ym"))

Final_factors <- Final_factors %>% mutate(SGP = ifelse(country == "SGP", 1, 0))
Final_factors <- Final_factors %>% mutate(KOR = ifelse(country == "KOR", 1, 0))
Final_factors <- Final_factors %>% mutate(TWN = ifelse(country == "TWN", 1, 0))

Final_factors <- Final_factors %>% filter(RET.USD != 0 & Beta != 0 & Momentum != 0)
Final_factors <- Final_factors %>% filter(!is.infinite(GPA) & !is.infinite(BM_m) & !is.infinite(NOA))

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

# Final_factors <- Final_factors %>% select(1:12)
# Final_factors <- data.table(Final_factors)


Ret <- Final_factors %>%
  mutate(Exp.RET = intercept+(BM_m*bm_m)+(GPA*gpa)+
           (Momentum*mom)+ sgp*SGP + kor*KOR + twn*TWN +
           bm_m_dummy.x*bm_m_dummy.y+profits_dummy.x*profits_dummy.y) %>% 
  select(Id,country,ym,RET.USD,Exp.RET) %>% as.data.table()

Ret <- Ret %>% drop_na()


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
lm(RET.USD~Exp.RET+as.factor(country), data = Ret)
