library(rstudioapi)
library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(PerfomanceAnalytcis)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Factors.R")



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

# Assign each stock to the respective quintile
create_breakpoints <- function(data, factor) {
  data[ , pf.bm := ifelse(data[[factor]]>bb80, "Big",
                          ifelse((data[[factor]]<=bb80 & data[[factor]]>bb60),"LessBig",
                                 ifelse((data[[factor]]<=bb60 & data[[factor]]>bb40),"Neutral",
                                        ifelse((data[[factor]]<=bb40 & data[[factor]]>bb20),"LessSmall",
                                               ifelse(data[[factor]]<=bb20,"Small",NA)))))]
  
}

# Calculate the value-weighted returns for the different factor portfolios
# calculate_factor_returns <- function(data, empty_df, factor) {
#   portfolio_returns <- data[!is.na(pf.bm)] %>% 
#     group_by(Date, pf.bm) %>% 
#     summarize(ret.port = weighted.mean(RET.USD, MV.USD.June)) %>% 
#     spread(pf.bm, ret.port) %>% mutate(hedge.pf = Big - Small) %>% 
#     rename("5" = Big, "4" = LessBig, "3" = Neutral,"2" = LessSmall, "1" =  Small, "5-1" = hedge.pf) %>% 
#     select(Date, "1", "2", "3", "4", "5", "5-1")
#   portfolio_returns <- as.data.table(portfolio_returns)
#   factor_returns <- colSums(portfolio_returns[,2:7], na.rm = T) / nrow(portfolio_returns)
#   empty_df <- rbind(empty_df, factor_returns)
#   empty_df
# }
calculate_factor_returns <- function(data, empty_df, factor) {
  
  namekey <- c(Big = "5", LessBig = "4", Neutral = "3", 
               LessSmall = "2", Small = "1", hedge.pf = "5-1", Date = "Date")
  portfolio_returns <- data[!is.na(pf.bm) & !is.na(RET.USD)] %>% # Some stocks are delisted but keep values from the prev fiscal y (dropping)
    group_by(Date, pf.bm) %>%                               
    summarize(ret.port = weighted.mean(RET.USD, MV.USD.June)) %>%
    spread(pf.bm, ret.port) %>% mutate(hedge.pf = Big - Small) # %>% 
  names(portfolio_returns) <- namekey[names(portfolio_returns)]
 
  portfolio_returns <- portfolio_returns %>% 
    #rename("5" = Big, "4" = LessBig, "3" = Neutral,"2" = LessSmall, "1" =  Small, "5-1" = hedge.pf) %>%
    select(Date, contains("1"), contains("2"), contains("3"), contains("4"), 
           contains("5"), contains("5-1"))
  #portfolio_returns <- as.data.table(portfolio_returns)
  #factor_returns <- colSums(portfolio_returns[,2:7], na.rm = T) / nrow(portfolio_returns)
  #empty_df <- rbind(empty_df, factor_returns)
  #empty_df
}

create_portfolio_sorts <- function(data, factor, empty_df) {
  factor_return <- create_quintiles(data, factor)
  factor_return <- create_breakpoints(factor_return, factor)
  empty_df <- calculate_factor_returns(factor_return, empty_df, factor)
}

# Create empty dataframe to display results
cols = c("1", "2", "3", "4", "5", "5-1")
portfolio_returns <- data.frame(matrix(nrow = 0, ncol = length(cols)))

# Exclude the stocks that have negative earnings, cash flows or gross profits
# We start with Book-To-Market and we only consider stocks with positive BM
# bm_factor <- factors %>% filter(BM>0)
# portfolio_returns <- create_portfolio_sorts(bm_factor, "BM", portfolio_returns)
# 
# #Test
# t <- create_quintiles(bm_factor, "BM")
# bp <- create_breakpoints(t, "BM")
# test <- calculate_factor_returns(bp, portfolio_returns, "BM")


source("FAT_RM.R")

# Filter for positive values of price ratios, ttest, alpha
t_test = data.table()
for (f in Yearly_factors_list){
  tmp_factor <- factors %>% filter(!!sym(f)>0)  
  tmp_factor <- tmp_factor %>% drop_na(!!sym(f))
  portfolio_returns <- create_portfolio_sorts(tmp_factor, f)
  t5minus1 <- unlist(t.test(portfolio_returns$`5-1`)[1])
  
  
  portfolio_returns$ym <-as.yearmon(portfolio_returns$Date)
  portfolio_returns <- merge(portfolio_returns[,c("ym","1", "2", "3", "4", "5", "5-1")], 
                             Market_Portfolio_FAT[,c("ym", "RMRF")], 
                             by="ym")
  Alpha = as.numeric(lm(`5-1`~RMRF, data = portfolio_returns)$coefficient[1])
  tAlpha= as.numeric(lm(`5-1`~RMRF, data = portfolio_returns)$coefficient[2])
  tt <- data.table(f, t5minus1, Alpha, tAlpha)
  t_test <- rbind(t_test, tt)
}

#Alpha = lm(`5-1`~RMRF, data = portfolio_returns)#$coefficient[1]
#print(Alpha)


#source("FAT_RM.R")

#portfolio_returns$ym <-as.yearmon(portfolio_returns$Date)
#hlp <- filter(Market_Portfolio_FAT)
#portfolio_returns <- merge(hlp[,c("RMRF", "ym")], 
#                           portfolio_returns[,c("1", "2", "3", "4", "5", "5-1", "ym")], by="ym")


