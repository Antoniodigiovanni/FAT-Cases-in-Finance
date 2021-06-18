library(rstudioapi)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Factors.R")

source("FAT_RM.R")



# Spanning tests for the model: BM_m, GPA, NOA, I/A, Size, Momentum, Beta
rm(Monthly_factors_list, Yearly_factors_list)
Factors_list_y <- c("GPA","NOA","Beta", "ItA", "NSI", "AG")
Factors_list_m <- c("BM_m", "Momentum")#,LMV", "CP_m")
# 1. Calculate the returns of the hedge portfolio built by splitting the factors
# into quintiles

create_quintiles <- function(data, factor) {
  hlpvariable2 <- data[month == 7 & !is.na(data[[factor]]) & pf.size == "Big"] %>% 
    group_by(country, year) %>% 
    summarize(bb20 = quantile(.data[[factor]], probs = c(0.2), na.rm = T),
              bb40 = quantile(.data[[factor]], probs = c(0.4), na.rm = T),
              bb60 = quantile(.data[[factor]], probs = c(0.6), na.rm = T),
              bb80 = quantile(.data[[factor]], probs = c(0.8), na.rm = T)) %>% 
    select(year, country, bb20, bb40, bb60, bb80)
  factors <- merge(data,hlpvariable2,
                   by.x=c("hcjun", "country"),
                   by.y=c("year", "country"))
}
create_quintiles_m <- function(data, factor) {
  hlpvariable2 <- data[!is.na(data[[factor]]) & pf.size == "Big"] %>% 
    group_by(country, year, month) %>% 
    summarize(bb20 = quantile(.data[[factor]], probs = c(0.2), na.rm = T),
              bb40 = quantile(.data[[factor]], probs = c(0.4), na.rm = T),
              bb60 = quantile(.data[[factor]], probs = c(0.6), na.rm = T),
              bb80 = quantile(.data[[factor]], probs = c(0.8), na.rm = T)) %>% 
    select(month, year, country, bb20, bb40, bb60, bb80)
  factors <- merge(data,hlpvariable2,
                   by.x=c("hcjun", "country", "month"),
                   by.y=c("year", "country", "month"))
}
create_breakpoints <- function(data, factor) {
  data[ , pf.bm := ifelse(data[[factor]]>bb80, "Big",
                          ifelse((data[[factor]]<=bb80 & data[[factor]]>bb60),"LessBig",
                                 ifelse((data[[factor]]<=bb60 & data[[factor]]>bb40),"Neutral",
                                        ifelse((data[[factor]]<=bb40 & data[[factor]]>bb20),"LessSmall",
                                               ifelse(data[[factor]]<=bb20,"Small",NA)))))]
  }
calculate_factor_returns <- function(data, empty_df, factor) {
  
  namekey <- c(Big = "5", LessBig = "4", Neutral = "3", 
               LessSmall = "2", Small = "1", hedge.pf = "5-1", Date = "Date")
  portfolio_returns <- data[!is.na(pf.bm) & !is.na(RET.USD)] %>% # Some stocks are delisted but keep values from the prev fiscal y (dropping)
    group_by(Date, pf.bm) %>%                               
    summarize(ret.port = weighted.mean(RET.USD, MV.USD.June, na.rm = T)) %>%
    spread(pf.bm, ret.port) %>% mutate(hedge.pf = Big - Small) # %>% 
  names(portfolio_returns) <- namekey[names(portfolio_returns)]
  
  portfolio_returns <- portfolio_returns %>%
    select(Date, contains("1"), contains("2"),
           contains("3"), contains("4"), 
           contains("5"), contains("5-1"))
}
create_portfolio_sorts <- function(data, factor, empty_df) {
  factor_return <- create_quintiles(data, factor)
  factor_return <- create_breakpoints(factor_return, factor)
  empty_df <- calculate_factor_returns(factor_return, empty_df, factor)
}
create_portfolio_sorts_monthly <- function(data, factor, empty_df) {
  factor_return <- create_quintiles_m(data, factor)
  factor_return <- create_breakpoints(factor_return, factor)
  empty_df <- calculate_factor_returns(factor_return, empty_df, factor)
} 


t_test = data.table()
factor_returns_y = data.table("ym" = unique(factors$ym))
Avg <- data.table()
for (f in Factors_list_y){
  tmp_factor <- factors %>% filter(!!sym(f)>0)  
  tmp_factor <- tmp_factor %>% drop_na(!!sym(f))
  portfolio_returns <- create_portfolio_sorts(tmp_factor, f)
  t5minus1 <- unlist(t.test(portfolio_returns$`5-1`)[1])
  
  ps <- portfolio_returns %>% ungroup() %>% 
    select(-Date) %>% colMeans(na.rm = T)
  Avg <- bind_rows(Avg, ps)
  
  
  portfolio_returns$ym <-as.yearmon(portfolio_returns$Date)
  portfolio_returns <- merge(portfolio_returns[,c("ym","1", "2", "3", "4", "5", "5-1")], 
                             Market_Portfolio_FAT[,c("ym", "RMRF")], 
                             by="ym")
  factor_returns_y <- left_join(factor_returns_y, portfolio_returns[,c("ym","5-1")], 
                                by = c("ym"))
  Alpha = as.numeric(lm(`5-1`~RMRF, data = portfolio_returns)$coefficient[1])
  tAlpha= as.numeric(summary(lm(`5-1`~RMRF, data = portfolio_returns))$coefficient[5])
  tt <- data.table(f, t5minus1, Alpha, tAlpha)
  t_test <- rbind(t_test, tt)
}
Factors_names <- c("ym", Factors_list_y)
names(factor_returns_y) <- Factors_names
t_test$ID <- seq.int(nrow(t_test))
Avg$ID <- seq.int(nrow(Avg))
sorted_portfolios_y <- left_join(t_test,
                                 Avg,
                                 by='ID') %>% 
  select(-ID)

rm(portfolio_returns, tt, tmp_factor)

t_test = data.table()
factor_returns_m = data.table("ym" = unique(factors$ym))
Avg <- data.table()
for (f in Factors_list_m){
  tmp_factor <- factors %>% filter(!!sym(f)>0)  
  tmp_factor <- tmp_factor %>% drop_na(!!sym(f))
  portfolio_returns <- create_portfolio_sorts_monthly(tmp_factor, f)
  t5minus1 <- unlist(t.test(portfolio_returns$`5-1`)[1])
  
  ps <- portfolio_returns %>% ungroup() %>% 
    select(-Date) %>% colMeans(na.rm = T)
  Avg <- bind_rows(Avg, ps)
  
  portfolio_returns$ym <-as.yearmon(portfolio_returns$Date)
  portfolio_returns <- merge(portfolio_returns[,c("ym","1", "2", "3", "4", "5", "5-1")], 
                             Market_Portfolio_FAT[,c("ym", "RMRF")], 
                             by="ym")
  
  factor_returns_m <- left_join(factor_returns_m, portfolio_returns[,c("ym","5-1")], 
                                by = c("ym"))
  Alpha = as.numeric(lm(`5-1`~RMRF, data = portfolio_returns)$coefficient[1])
  tAlpha= as.numeric(summary(lm(`5-1`~RMRF, data = portfolio_returns))$coefficient[5])
  tt <- data.table(f, t5minus1, Alpha, tAlpha)
  t_test <- rbind(t_test, tt)
}
Factors_names <- c("ym", Factors_list_m)
names(factor_returns_m) <- Factors_names
t_test$ID <- seq.int(nrow(t_test))
Avg$ID <- seq.int(nrow(Avg))
sorted_portfolios_m <- left_join(t_test,
                                 Avg,
                                 by='ID') %>% 
  select(-ID)

factor_returns <- merge(factor_returns_m, factor_returns_y, by = "ym", all = T)


rm(t_test, factor_returns_m, factor_returns_y, Alpha, ps, t5minus1, tAlpha, f,
   tt, tmp_factor, portfolio_returns, Avg, Factors_list_m, Factors_list_y, Factors_names)

# Factors - Beta, BM_m, GPA, NOA, Momentum 
# Perform Spanning Regressions

factor_returns[,summary(lm(BM_m ~ GPA + NOA + Momentum +  Beta))]

# GPA's intercept is significantly different from zero
factor_returns[,summary(lm(GPA ~BM_m + NOA  + Momentum + Beta))]


# The same for NOA, intercept not significantly different from zero
factor_returns[,summary(lm(NOA ~ BM_m + GPA + Momentum + Beta))]

# No significant difference from zero in Beta 
factor_returns[,summary(lm(Beta ~  GPA + BM_m + Momentum))]

# Intercept of momentum is significantly different from zero
factor_returns[,summary(lm(Momentum ~ BM_m + GPA + NOA + Beta))]

# Try without Beta and Momentum

factor_returns[,summary(lm(BM_m ~ GPA + NOA))]
factor_returns[,summary(lm(GPA ~BM_m + NOA  ))]
factor_returns[,summary(lm(NOA ~ BM_m + GPA ))]


# Dropping some factors
# Dropping ItA, NOA is still not significant, but dropping Beta it becomes significant
factor_returns[,summary(lm(NOA ~ BM_m + LMV + GPA + Momentum))]

factor_returns[,summary(lm(Beta ~ BM_m + LMV + GPA + Momentum))]

# Drop Beta and NOA


