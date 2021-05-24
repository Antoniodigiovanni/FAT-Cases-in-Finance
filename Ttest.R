library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

source("Factors.R")

portfolio_returns <- as.data.table(portfolio_returns)

+create_quintiles <- function(data, factor) {
  hlpvariable2 <- data[month == 7 & !is.na(data[[factor]]) & pf.size == "Big"] %>% group_by(country, year) %>% 
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
calculate_factor_returns <- function(data, empty_df, factor) {
  portfolio_returns <- data[!is.na(pf.bm)] %>% 
    group_by(Date, pf.bm) %>% 
    summarize(ret.port = weighted.mean(RET.USD, MV.USD.June)) %>% 
    spread(pf.bm, ret.port) %>% mutate(hedge.pf = Big - Small) %>% 
    rename("5" = Big, "4" = LessBig, "3" = Neutral,"2" = LessSmall, "1" =  Small, "5-1" = hedge.pf) %>% 
    select(Date, "1", "2", "3", "4", "5", "5-1")
  portfolio_returns <- as.data.table(portfolio_returns)
  factor_returns <- colSums(portfolio_returns[,2:7], na.rm = T) / nrow(portfolio_returns)
  empty_df <- rbind(empty_df, factor_returns)
  empty_df
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
bm_factor <- factors %>% filter(BM>0)
portfolio_returns <- create_portfolio_sorts(bm_factor, "BM", portfolio_returns)
# Filter out stocks with negative earnings 
ep_factor <- factors %>% filter(EP > 0) %>% drop_na(EP)
portfolio_returns <- create_portfolio_sorts(ep_factor, "EP", portfolio_returns)
# Filter out stocks with negative CF
cp_factor <- factors %>% filter(CP > 0) %>% drop_na(CP)
portfolio_returns <- create_portfolio_sorts(cp_factor, "CP", portfolio_returns)

roe_factor <- factors %>% filter(ROE > 0)
portfolio_returns <- create_portfolio_sorts(roe_factor, "ROE", portfolio_returns)

roa_factor <- factors %>% filter(ROA > 0)
portfolio_returns <- create_portfolio_sorts(roa_factor, "ROA", portfolio_returns)

gpa_factor <- factors %>% filter(GPA > 0)
portfolio_returns <- create_portfolio_sorts(gpa_factor, "GPA", portfolio_returns)

opbe_factor <- factors %>% filter(OPBE > 0)
portfolio_returns <- create_portfolio_sorts(opbe_factor, "OPBE", portfolio_returns)

oa_factor <- factors %>% drop_na(OA)
portfolio_returns <- create_portfolio_sorts(oa_factor, "OA", portfolio_returns)

colnames(portfolio_returns) <- cols
rows <- c("BM", "EP", "CP", "ROE", "ROA", "GP/A", "OP/BE", "OA")
rownames(portfolio_returns) <- rows


#ttest
CS.reg.estimtates.contr.2[,t.test(gamma_zero)]






