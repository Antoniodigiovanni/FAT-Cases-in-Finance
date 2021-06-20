library(tseries)
#require(DEoptim)
library(rstudioapi)


# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 
source("Factors.R")

factor_port <- factors
# Removes all the Inf observations
factor_port <- factor_port %>% filter(across(everything(), ~ !is.infinite(.x)))
factor_port <- factor_port %>% filter(pf.size == "Big" & ym > "Jun 1998") %>% 
  select(Id,Date, country, LMV.USD, RET.USD, RET, ym, Beta, BM_m, GPA, NOA, Momentum, pf.size) %>% 
  filter(across(everything(), ~ !is.na(.x)))
#test <- test %>% mutate(fac_score = 0.5*BM_m + 0.5*GPA) %>% group_by(ym) %>%
  #summarise(n_obs = n()) %>% arrange(fac_score) %>% drop_na()

# Factors that are included in the model
fac <- c("Beta", "BM_m", "GPA", "NOA", "Momentum")
N <- length(fac)

# Equal weighting of the factors
# Not sure if minus beta and minus NOA is correct
# First standardize using z-scores
factor_port <- factor_port %>% mutate(Beta_norm = (Beta - mean(Beta))/sd(Beta),
                                      BM_m_norm = (BM_m - mean(BM_m))/sd(BM_m),
                                      GPA_norm = (GPA - mean(GPA))/sd(GPA),
                                      NOA_norm = (NOA - mean(NOA))/sd(NOA),
                                      Mom_norm = (Momentum - mean(Momentum))/sd(Momentum))
factor_port <- factor_port %>% mutate(fac_score = 1/N*-Beta_norm + 1/N*BM_m_norm + 1/N*GPA_norm + 1/N*-NOA_norm + 1/N*Mom_norm) %>% 
  group_by(ym) %>% arrange(ym, desc(fac_score)) %>% 
  filter(ym > "Jun 1998" & !is.nan(fac_score))

counts <- factor_port %>% summarise(n_obs = n())

factor_port <- merge(factor_port, counts, by="ym")

quint <- factor_port %>% group_by(ym) %>% summarize(top20 = quantile(fac_score, probs = c(0.2), na.rm = T),
                            top40 = quantile(fac_score, probs = c(0.4), na.rm = T),
                            top60 = quantile(fac_score, probs = c(0.6), na.rm = T),
                            top80 = quantile(fac_score, probs = c(0.8), na.rm = T)) %>% 
  select(ym, top20, top40, top60, top80)

factor_port <- merge(factor_port, quint, by = "ym")

factor_port <- data.table(factor_port)

factor_port <- factor_port[ , bucket := ifelse(fac_score>top80, 1,
                        ifelse(fac_score<=top80 & fac_score>top60,2,
                               ifelse(fac_score<=top60 & fac_score>top40,3,
                                      ifelse(fac_score<=top40 & fac_score>top20,4,
                                             ifelse(fac_score<=top20,5,NA)))))]


inv_universe <- factor_port %>% filter(bucket == 1 | bucket == 2) %>% group_by(ym) %>% arrange(ym, desc(fac_score)) %>% select(-n_obs)
univ_count <- inv_universe %>% summarise(n_obs = n())
inv_universe <- merge(inv_universe, univ_count)

rm(factor_port, quint, counts)

# Only invest in the first bucket and do equal weighting
eq_port <- inv_universe %>% group_by(ym) %>%  mutate(weights = 1 / n_obs)
eq_port <- eq_port %>% mutate(weighted_return = weights*RET)
eq_cumreturn <- eq_port %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return))
Eq_factor_portfolio <- eq_cumreturn %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Eq_factor_portfolio$Portfolio_Value[1] <- 100

save(Eq_factor_portfolio, file = "EW_Portfolio.RData")

# New turnover calculation
turnover_ew <- eq_port %>% select(ym, Id, country, LMV.USD, RET.USD, RET, weights, n_obs) %>% mutate(weights = weights*100)
turnover_ew <- turnover_ew %>% mutate(wlm = weights * (RET.USD/100 + 1))
dfh <- turnover_ew %>% summarise(sum_wlm = sum(wlm))
turnover_ew <- merge(turnover_ew, dfh, by = "ym")
turnover_ew <- turnover_ew %>% mutate(new_weights = (wlm/sum_wlm)*100)
turnover_ew <- turnover_ew %>% ungroup
turnover_ew <- turnover_ew %>% group_by(Id)
turnover_ew <- turnover_ew %>% mutate(lym = lag(ym),
                        wlm = ifelse(is.na(wlm), 0, lag(new_weights)),
                        lym = ifelse(is.na(lym), 0, lym))

#check <- test %>% filter(Id == "13117D")

#check <- check %>% mutate(cwg = ifelse(ym - 1/12 == lym, abs(weights - wlm), weights))
turnover_ew <- turnover_ew %>% mutate(cwg = ifelse(ym - 1/12 == lym, abs(weights - wlm), weights))
T <- turnover_ew %>% group_by(ym) %>% summarise(t = n())
T <- nrow(T)


turnover_ew <- turnover_ew %>% ungroup %>% summarise(turnover = sum(cwg)/T)

Weights_df <- eq_port %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N_EW <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N_EW <- Effective_N_EW/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
Eq_factor_portfolio <- Eq_factor_portfolio %>% mutate(Year = year(ym))
sd_dev <- sd(Eq_factor_portfolio$monthly_ret)*sqrt(12)
Yearly_ret <- Eq_factor_portfolio %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_EW <- Yearly_ret
SR_EW <- SR_EW %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_EW <- SR_EW %>% mutate(sharpe_ratio = (ret-rf)/sd)

# Hit Rate
hit_rate <- eq_port %>% filter(weights > 0) %>% mutate(hit = ifelse(RET>0, 1, 0))
hit_rate <- hit_rate %>% ungroup %>% summarise(hit_rate = sum(hit) / n())

# Average Number of Stocks
number_stocks <- eq_port %>% filter(weights > 0) %>% summarise(n_stocks = n())
mean(number_stocks$n_stocks)

IR <- merge(Eq_factor_portfolio, Portfolio_Returns, by = "ym")
sd_dev <- sd(IR$monthly_ret.x)*sqrt(12)
IR <- IR %>% summarise(sd = sd_dev, ret = (mean(ret.x)-1)*100, bench = (mean(ret.y)-1)*100)
IR <- IR %>% mutate(information_ratio = ((ret-bench)/sd))

# Maximum Drawdown
rets <- pull(Eq_factor_portfolio, Portfolio_Value)
rets <- xts(Eq_factor_portfolio$Portfolio_Value, order.by = Eq_factor_portfolio$ym)
MD <- maxdrawdown(rets)
rets[MD$from]
rets[MD$to]
drawdown <- (554.9008 - 1180.962) / 1180.962

MD <- -min(Eq_factor_portfolio$ret)

# Now try the MV
total_mv_yearly <- inv_universe %>% group_by(ym) %>% 
  summarise(mv_total = sum(LMV.USD))
vw_port <- merge(inv_universe, total_mv_yearly, by = "ym")
# Weights are calculated by MV
vw_port <- vw_port %>% group_by(ym)  %>%  
  mutate(weights = LMV.USD / mv_total,
         weights = ifelse(weights < 0.0001, 0, weights),
         weights = weights / sum(weights),
          cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_vw <- vw_port %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return)) %>% drop_na()
VW_Factor_Portfolio <- cum_return_vw %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
VW_Factor_Portfolio$Portfolio_Value[1] <- 100

# New turnover calculation for value-weighted pf
turnover_vw <- vw_port %>% select(ym, Id, country, LMV.USD, RET.USD, RET, weights, n_obs) %>% mutate(weights = weights*100)
turnover_vw <- turnover_vw %>% mutate(wlm = weights * (RET.USD/100 + 1))
hdf <- turnover_vw %>% summarise(sum_wlm = sum(wlm))
turnover_vw <- merge(turnover_vw, hdf, by = "ym")
turnover_vw <- turnover_vw %>% mutate(new_weights = (wlm/sum_wlm)*100)
turnover_vw <- turnover_vw %>% ungroup
turnover_vw <- turnover_vw %>% group_by(Id)
turnover_vw <- turnover_vw %>% mutate(lym = lag(ym),
                        wlm = ifelse(is.na(wlm), 0, lag(new_weights)),
                        lym = ifelse(is.na(lym), 0, lym))

check <- turnover_vw %>% filter(Id == "13117D")

check <- check %>% mutate(cwg = ifelse(ym - 1/12 == lym, abs(weights - wlm), weights))
turnover_vw <- turnover_vw %>% mutate(cwg = ifelse(ym - 1/12 == lym, abs(weights - wlm), weights))
T <- turnover_vw %>% group_by(ym) %>% summarise(t = n())
T <- nrow(T)
transactioncost <- turnover_vw %>% ungroup %>% group_by(ym) %>% summarise(costs = sum(cwg)/100)
sum(transactioncost$costs)
mean(transactioncost$costs)

turnover_vw <- turnover_vw %>% ungroup %>% summarise(turnover = sum(cwg)/T)

# Yearly turnover (as per Hanauer, Lauterbach (2019))
Weights_df <- vw_port %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N_VW <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N_VW <- Effective_N_VW/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
VW_Factor_Portfolio <- VW_Factor_Portfolio %>% mutate(Year = year(ym))
sd_dev <- sd(VW_Factor_Portfolio$monthly_ret)*sqrt(12)
Yearly_ret <- VW_Factor_Portfolio %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_vw <- Yearly_ret
SR_vw <- SR_vw %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_vw <- SR_vw %>% mutate(sharpe_ratio = (ret-rf)/sd)

top10_vw <- vw_port %>% group_by(ym) %>% arrange(desc(weights)) %>% slice_head(n = 10)
top10_vw <- top10_vw %>% summarise(avg_weight = mean(weights)) 
mean(top10_vw$avg_weight)


# Maximum Drawdown

MD <- -min(VW_Factor_Portfolio$ret)


# Lets try fac_score weighted
total_fac_score <- inv_universe %>% group_by(ym) %>% summarise(fac_total = sum(fac_score))
fac_weighted <- merge(inv_universe, total_fac_score, by = "ym")
fac_weighted <- fac_weighted %>% group_by(ym)  %>%  
  mutate(weights = fac_score / fac_total,
         weights = ifelse(weights < 0.0001, 0, weights),
         weights = weights / sum(weights),
         cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_fs <- fac_weighted %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return)) %>% drop_na()
Fac_weighted_Portfolio <- cum_return_fs %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Fac_weighted_Portfolio$Portfolio_Value[1] <- 100

# Yearly turnover (as per Hanauer, Lauterbach (2019))
Weights_df <- fac_weighted %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))
to <- function(weight){
  result <- abs(lead(weight) - weight)
}
Turnover_df <- transposed_Weights %>% mutate(across( .fns = to)) %>% drop_na()
Turnover <- sum(Turnover_df)/(2*T)

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- Effective_N/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
Fac_weighted_Portfolio <- Fac_weighted_Portfolio %>% mutate(Year = year(ym))
sd_dev <- sd(Fac_weighted_Portfolio$monthly_ret)*sqrt(12)
Yearly_ret <- Fac_weighted_Portfolio %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_fw <- Yearly_ret
SR_fw <- SR_fw %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_fw <- SR_fw %>% mutate(sharpe_ratio = (ret-rf)/sd)

top10 <- fac_weighted %>% group_by(ym) %>% arrange(desc(weights)) %>% slice_head(n = 10)
top10 <- top10 %>% summarise(avg_weight = mean(weights)) 
mean(top10$avg_weight)

# Now try the MV but limit maximum weights to 10%
total_mv_yearly <- inv_universe %>% group_by(ym) %>% 
  summarise(mv_total = sum(LMV.USD))
vw_port <- merge(inv_universe, total_mv_yearly, by = "ym")

vw_port <- vw_port %>% group_by(ym)  %>%  
  mutate(weights = LMV.USD / mv_total)

# Limiting weights to 10%
Weight_limit <- 0.1
vw_temp <- vw_port
vw_checked <- data.table()


while (max(vw_temp$weights) > Weight_limit) {
  vw_big <- vw_temp %>% group_by(ym) %>% filter(weights>Weight_limit) %>% 
    mutate(weights = 0.1)
  vw_checked <- rbind(vw_checked, vw_big)
  nBigWeight <- vw_checked %>% group_by(ym) %>% count() %>% rename(number = "n")
  vw_temp <- left_join(vw_temp, nBigWeight, by="ym") %>% mutate(number = ifelse(is.na(number), 0, number))
  vw_temp <- vw_temp %>% 
    filter(weights < Weight_limit) 
  vw_temp <- vw_temp %>%
    group_by(ym) %>% 
    mutate(mv_total = sum(LMV.USD)) #%>% 
  vw_temp <- vw_temp %>%
    mutate(weights = LMV.USD / mv_total * (1-Weight_limit*number)) %>% 
    select(-c("number"))
  
  
}
vw_port <- rbind(vw_temp, vw_checked)
glimpse(vw_port)

rm(vw_temp, vw_checked)

vw_port <- vw_port %>% group_by(ym) %>%  mutate(weighted_return = weights * RET.USD)


cum_return_vw <- vw_port %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return)) %>% drop_na()
VW_Factor_Portfolio <- cum_return_vw %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
VW_Factor_Portfolio$Portfolio_Value[1] <- 100

# Yearly turnover (as per Hanauer, Lauterbach (2019))
Weights_df <- vw_port %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))
to <- function(weight){
  result <- abs(lead(weight) - weight)
}
Turnover_df <- transposed_Weights %>% mutate(across( .fns = to)) %>% drop_na()
Turnover <- sum(Turnover_df)/(2*T)

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- Effective_N/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
VW_Factor_Portfolio <- VW_Factor_Portfolio %>% mutate(Year = year(ym))
sd_dev <- sd(VW_Factor_Portfolio$monthly_ret)*sqrt(12)
Yearly_ret <- VW_Factor_Portfolio %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_vw <- Yearly_ret
SR_vw <- SR_vw %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_vw <- SR_vw %>% mutate(sharpe_ratio = (ret-rf)/sd)

# Hit Rate
hit_rate <- vw_port %>% filter(weights > 0) %>% mutate(hit = ifelse(RET>0, 1, 0))
hit_rate <- hit_rate %>% ungroup %>% summarise(hit_rate = sum(hit) / n())

# Average Number of Stocks
number_stocks <- vw_port %>% filter(weights > 0) %>% summarise(n_stocks = n())
mean(number_stocks$n_stocks)

top10 <- vw_port %>% group_by(ym) %>% arrange(desc(weights)) %>% slice_head(n = 10)
top10 <- top10 %>% summarise(avg_weight = mean(weights)) 
mean(top10$avg_weight)

MD <- -min(VW_Factor_Portfolio$ret)
write.csv(VW_Factor_Portfolio, "vw_portfolio_limit.csv")



# Check MV return of all stocks
factor_port <- factors
# Removes all the Inf observations
factor_port <- factor_port %>% filter(across(everything(), ~ !is.infinite(.x)))
factor_port <- factor_port %>% filter(pf.size == "Big" & ym > "Jun 1998") %>% 
  select(Id, country, LMV.USD, RET.USD, RET, ym, Beta, BM_m, GPA, NOA, Momentum, pf.size) %>% 
  filter(across(everything(), ~ !is.na(.x))) %>% arrange(ym)

total_mv_yearly <- factor_port %>% group_by(ym) %>% 
  summarise(mv_total = sum(LMV.USD))
mpf <- merge(factor_port, total_mv_yearly, by = "ym")
mpf <- mpf %>% group_by(ym)  %>%  
  mutate(weights = LMV.USD / mv_total,
         cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_mpf <- mpf %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return)) %>% drop_na()
 

Portfolio_Returns <- cum_return_mpf %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Portfolio_Returns$Portfolio_Value[1] <- 100

save(Portfolio_Returns, file = "Market_Portfolio.RData")

write.csv(Portfolio_Returns, "vw_market.csv")
write.csv(Eq_factor_portfolio, "eq_port.csv")

# Yearly turnover (as per Hanauer, Lauterbach (2019))
Weights_df <- mpf %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))
to <- function(weight){
  result <- abs(lead(weight) - weight)
}
Turnover_df <- transposed_Weights %>% mutate(across( .fns = to)) %>% drop_na()
Turnover <- sum(Turnover_df)/(2*T)

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- Effective_N/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
Portfolio_Returns <- Portfolio_Returns %>% mutate(Year = year(ym))
sd_dev <- sd(Portfolio_Returns$monthly_ret)*sqrt(12)
Yearly_ret <- Portfolio_Returns %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_market <- Yearly_ret
SR_market <- SR_market %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_market <- SR_market %>% mutate(sharpe_ratio = (ret-rf)/sd)

top10 <- vw_port %>% group_by(ym) %>% arrange(desc(weights)) %>% slice_head(n = 10)
top10 <- top10 %>% summarise(avg_weight = mean(weights)) 
mean(top10$avg_weight)

IR <- merge(VW_Factor_Portfolio, Portfolio_Returns, by = "ym")
sd_dev <- sd(IR$monthly_ret.x)*sqrt(12)
IR <- IR %>% summarise(sd = sd_dev, ret = (mean(ret.x)-1)*100, bench = (mean(ret.y)-1)*100)
IR <- IR %>% mutate(information_ratio = ((ret-bench)/sd))


# Check EW return of all stocks
factor_port <- factors
# Removes all the Inf observations
factor_port <- factor_port %>% filter(across(everything(), ~ !is.infinite(.x)))
factor_port <- factor_port %>% filter(pf.size == "Big" & ym > "Jun 1998") %>% 
  select(Id, country, LMV.USD, RET.USD, RET, ym, Beta, BM_m, GPA, NOA, Momentum, pf.size) %>% 
  filter(across(everything(), ~ !is.na(.x))) %>% arrange(ym)

n_obs <- factor_port %>% group_by(ym) %>% summarise(n_obs = n())

ew_mpf <- merge(factor_port, n_obs, by = "ym")
ew_mpf <- ew_mpf %>% group_by(ym)  %>%  
  mutate(weights = 1 / n_obs,
         cum_weights = cumsum(weights),
         weighted_return = weights * RET.USD)

cum_return_ew_mpf <- ew_mpf %>% group_by(ym) %>% summarise(monthly_ret = sum(weighted_return)) %>% drop_na()


Portfolio_Returns_EW <- cum_return_ew_mpf %>% arrange(ym) %>%
  mutate(ret = 1+monthly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Portfolio_Returns_EW$Portfolio_Value[1] <- 100

write.csv(Portfolio_Returns_EW, "ew_market.csv")

write.csv(Portfolio_Returns, "vw_market.csv")
write.csv(Eq_factor_portfolio, "eq_port.csv")

# Yearly turnover (as per Hanauer, Lauterbach (2019))
Weights_df <- mpf %>% group_by(ym) %>% select(Id, ym, weights) %>% spread(ym, weights)
Weights_df <- Weights_df %>% replace(is.na(.),0)
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.factor), as.character))
transposed_Weights <- transposed_Weights %>% mutate(across(where(is.character), as.numeric))
to <- function(weight){
  result <- abs(lead(weight) - weight)
}
Turnover_df <- transposed_Weights %>% mutate(across( .fns = to)) %>% drop_na()
Turnover <- sum(Turnover_df)/(2*T)

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- ((weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- Effective_N/T

# Sharpe Ratio
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF))
Portfolio_Returns <- Portfolio_Returns %>% mutate(Year = year(ym))
sd_dev <- sd(Portfolio_Returns$monthly_ret)*sqrt(12)
Yearly_ret <- Portfolio_Returns %>% group_by(Year) %>% summarise(Yret = prod(ret))
Yearly_ret <- merge(Yearly_ret, FF, by = "Year")
SR_market <- Yearly_ret
SR_market <- SR_market %>% summarise(sd = sd_dev, ret = (mean(Yret)-1)*100, rf = (mean(YRF)-1)*100)
SR_market <- SR_market %>% mutate(sharpe_ratio = (ret-rf)/sd)

top10 <- vw_port %>% group_by(ym) %>% arrange(desc(weights)) %>% slice_head(n = 10)
top10 <- top10 %>% summarise(avg_weight = mean(weights)) 
mean(top10$avg_weight)

IR <- merge(Eq_factor_portfolio, Portfolio_Returns, by = "ym")
sd_dev <- sd(IR$monthly_ret.x)*sqrt(12)
IR <- IR %>% summarise(sd = sd_dev, ret = (mean(ret.x)-1)*100, bench = (mean(ret.y)-1)*100)
IR <- IR %>% mutate(information_ratio = ((ret-bench)/sd))



# total_mv_yearly <- test_port %>% group_by(ym) %>% 
#   summarise(mv_total = sum(MV.USD))
# test_port <- merge(test_port, total_mv_yearly, by = "ym")
# test_port <- test_port %>% group_by(ym)  %>%  
#   mutate(weights = MV.USD / mv_total,
#          cum_weights = cumsum(weights),
#          weighted_return = weights * RET.USD)
# 
# test <- test_port %>% group_by(ym) %>% summarise(monthly_return = mean(RET.USD))
# test2 <- test_port %>% group_by(ym) %>% summarise(monthly_weighted_return = mean(weighted_return))
# 
# test_return <- test_port %>% group_by(ym) %>% summarise(yearly_ret = sum(weighted_return))
# cum_return_mpf <- cum_return_mpf %>% mutate(Ret = (yearly_ret/100+1)) %>% summarise(Ret = prod(Ret) -1) 
# 
# Portfolio_Returns <- test_port %>% arrange(ym) %>%
#   mutate(ret = 1+yearly_ret/100) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
# Portfolio_Returns$Portfolio_Value[1] <- 100



##TE and IR
source("MSCI.R")
MSCI_annRet <- (tail(MSCI_weighted$Portfolio_Value, n=1)/100)^(1/(2018-1998+1))-1
EW_annRet <- (tail(Portfolio_Returns_EW$Portfolio_Value, n=1)/100)^(1/(2018-1998+1))-1
VW_annRet <- (tail(Portfolio_Returns$Portfolio_Value, n=1)/100)^(1/(2018-1998+1))-1

MSCI_monthly$ym <- as.yearmon(MSCI_monthly$Date)

IR_ew<-merge(Portfolio_Returns_EW, MSCI_monthly[,c("ym", "MSCI_ret")], by="ym")
IR_ew$SqDif <-(IR_ew$monthly_ret-IR_ew$MSCI_ret)^2
IR_vw<-merge(Portfolio_Returns, MSCI_monthly[,c("ym", "MSCI_ret")], by="ym")
IR_vw$SqDif <-(IR_vw$monthly_ret-IR_vw$MSCI_ret)^2



##Tracking Error
retPeriods <- 2018-1995+1-1

#IR$help <-IR$portfolio_ret-InfRatio$YMR
TE_ew <- as.data.frame(sqrt(sum(IR_ew$SqDif))/retPeriods)
colnames(TE_ew)[1]<-"TrackingError-EW"
TE_vw <- as.data.frame(sqrt(sum(IR_vw$SqDif))/retPeriods)
colnames(TE_vw)[1]<-"TrackingError-VW"

Results_TE <- merge(TE_ew, TE_vw)


InfRatio_ew <- as.data.frame((EW_annRet-MSCI_annRet)/Results_TE$`TrackingError-EW`[1]*100)
colnames(InfRatio_ew)[1]<-"Information Ratio-EW"
InfRatio_vw <- as.data.frame((VW_annRet-MSCI_annRet)/Results_TE$`TrackingError-VW`[1]*100)
colnames(InfRatio_vw)[1]<-"Information Ratio-VW"

Results_IR <- merge(InfRatio_ew, InfRatio_vw)


