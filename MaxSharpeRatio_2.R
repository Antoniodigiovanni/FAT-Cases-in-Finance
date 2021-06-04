################################################################################
#                                                                              #
# Do not run the entire script! It requires a Matlab script before the second  #
# loop can be executed, run until line 54 and then move to Matlab              #
#                                                                              #
################################################################################

library(rstudioapi)
library(rapportools)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"))
#source("ReturnForecast.R")

# Reconstruction date -> July of each year
# matrix based on the excess returns of the previous 36 months excess returns

# Stocks selection
stocks <- all_data %>% select(Id, country, Date, ym, year, month, RET, MV, pf.size)
stocks <- left_join(stocks,
                    FF[,c("ym","RF")],
                    by="ym")



# Still not ordering by Market Cap (less than 1000 big stocks (with complete obs) each year, maybe we use all of them?)

Years_list <- unique(stocks$year)
write.csv(Years_list, file = file.path("Max_Sharpe","Years_list.csv"))

for (y in Years_list){
  print(y)
  Cov_prep <- stocks %>% mutate(Excess.RET = RET/100-RF) %>% select(Id,ym,Excess.RET, MV, pf.size)
  Investable_Ids <- stocks %>% filter(ym == as.yearmon(paste0("Jun", year(as.yearmon(y)))) & pf.size=="Big" & !is.na(RET))
  #only invest if still exist after one month & has return values over last 36 months ->otherwise not mature enough to calc covariance matrix
  Exist_in_July <- stocks %>% filter(ym <=as.yearmon(paste0("Jul", year(as.yearmon(y))))& 
                                       ym >= (as.yearmon(paste0("Jul", year(as.yearmon(y))))-36/12) & !is.na(RET))
  Cov_prep <- Cov_prep %>% filter(ym < as.yearmon(paste0("Jul", year(as.yearmon(y)))) & 
                                    ym >= (as.yearmon(paste0("Jul", year(as.yearmon(y))))-36/12)) %>% 
    arrange(ym,-MV) %>% filter(Id %in% Investable_Ids$Id & Id %in% Exist_in_July$Id) %>% select(-pf.size,-MV) # Still not selecting the first 1000 by market cap
  
  Cov_spread <- Cov_prep %>%  spread(Id, Excess.RET) %>% select(-ym)
  
  Ret_temp <- Ret %>% filter(ym >= as.yearmon(paste0("Jul", year(as.yearmon(y)))) &
                               ym < (as.yearmon(paste0("Jul", year(as.yearmon(y))))+1) &  # Check that forecasted returns for delisted 
                               Id %in% Investable_Ids$Id & Id %in% Exist_in_July$Id) %>%  # stocks do not get deleted
    mutate(RET = (Exp.RET+1)) %>% 
    group_by(Id) %>% 
    summarise(RET = prod(RET))
  # We require the stock to have 36 returns in the timeframe of 36 months,
  # so if a column has at least one NA for this timeframe, we drop it
  Cov_spread <- Filter(function(x)!any(is.na(x)), Cov_spread)
  
  # Filter the Ids in Ret with the columns in Cov_spread
  Ret_t <- Ret_temp %>% filter(Id %in% row.names(t(Cov_spread)))
  #Cov_spread <- Cov_spread[, apply(Cov_spread, 2, function(x) !any(is.na(x)))]
  Cov_spread_transpose <- as.data.frame(t(Cov_spread))
  Cov_spread_transpose<- Cov_spread_transpose %>% filter(row.names(Cov_spread_transpose) %in% Ret_t$Id)
  Cov_spread_t <- as.data.frame(t(Cov_spread_transpose))

  if (!is_empty(Cov_spread_t) & !is.empty(Ret_t)){
    cov_m <- cov(Cov_spread_t, use = "pairwise.complete.obs")
    cov_m <- as.data.frame(cov_m)
    write.csv(Ret_t, file = file.path("Max_Sharpe", paste0("Ret_",y,".csv")))
    write.csv(cov_m, file = file.path("Max_Sharpe",paste0("cov_",y,".csv")))
  }
  
  
  
}
rm(y, cov_m, Cov_prep, Cov_spread)



###########################################################
#                                                         #
# Perform optimization on Matlab, in ./Min_Var/Min_Var.m  #
#                                                         #
###########################################################

# Afterwards load the csv files with the optimal weights.
Portfolio_Returns <- data.table()
for (y in Years_list){
  
  # Loading Portfolio Weights
  tryCatch({
    # Create one df for each year with the following line
    #assign(paste0("Weight_",y),read_csv(file.path("Min_Var",paste0("weights_", y,".csv"))))
    
    Weight <- read_csv(file.path("Max_Sharpe",paste0("weights_", y,".csv")))
    # Normalizing weights (as weights of 10^-5 and lower have been set to 0)
    #Weight <-as.data.frame(Weight)
    
    #Weight <- Weight %>% rename(Id =  %>% mutate(Id = as.character(Id), 
    #                                                   x_vect = x_vect/sum(x_vect)) %>% 
    #  arrange(-x_vect)
    names(Weight)[names(Weight)=="Row"] <- c("Id")
    Weight <- Weight%>% mutate(Id = as.character(Id), 
                                                 x_vect = x_vect/sum(x_vect)) %>% 
                                                   arrange(-x_vect)
    #print(y)
    #print(nrow(Weight))
    #print("\n")
    
    # Perform calculations on portfolios or create some df with all the Ids and the
    # weights for each year ...
    stocks_ret <- stocks %>% 
      filter(ym >= as.yearmon(paste0("Jul", year(as.yearmon(y)))) &
               ym < (as.yearmon(paste0("Jul", year(as.yearmon(y))))+1))
    stocks_ret <- stocks_ret %>% filter(Id %in% Weight$Id) %>% mutate(RET = (RET/100+1)) %>% 
      group_by(Id) %>% 
      summarise(Ret = prod(RET, na.rm = T)) %>% mutate(Ret = Ret-1)
    portfolio <- left_join(Weight, stocks_ret, by="Id")
    portfolio <- portfolio %>% arrange(-x_vect)
    #portfolio_ret <- portfolio %>% 
    portfolio_ret <- sum(portfolio$Ret*portfolio$x_vect, na.rm=TRUE)
    portfolio_ret <- as.data.frame(cbind(y, portfolio_ret))
    Portfolio_Returns <- as.data.table(rbind(Portfolio_Returns, portfolio_ret))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

Portfolio_Returns <- Portfolio_Returns %>% arrange(y) %>%
  mutate(ret = 1+portfolio_ret) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Portfolio_Returns$Portfolio_Value[1] <- 100


