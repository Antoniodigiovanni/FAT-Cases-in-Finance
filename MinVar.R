################################################################################
#                                                                              #
# Do not run the entire script! It requires a Matlab script before the second  #
# loop can be executed, run until line 54 and then move to Matlab              #
#                                                                              #
################################################################################

library(rstudioapi)


# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")
source("FAT_RM.R")

# Reconstruction date -> July of each year
# We select the first N=1000 stocks for each year and calculate the covariance 
# (less than 1000 stocks present on average, so we take all of them)
# matrix based on the excess returns of the previous 36 months (60 in the paper)  excess returns

# Stocks selection
stocks <- all_data %>% select(Id, country, Date, ym, year, month, RET, MV, pf.size)
stocks <- left_join(stocks,
                    Market_Portfolio_FAT[,c("ym","RF")],
                    by="ym")



# Still not ordering by Market Cap (less than 1000 big stocks (with complete obs) each year, maybe we use all of them?)

Years_list <- unique(stocks$year)
write.csv(Years_list, file = file.path("Min_Var","Years_list.csv"))

for (y in Years_list){
  print(y)
  Cov_prep <- stocks %>% mutate(Excess.RET = RET/100-RF) %>% select(Id,ym,Excess.RET, MV, pf.size)
  Cov_prep <- Cov_prep %>% filter(ym < as.yearmon(paste0("Jul", year(as.yearmon(y)))) & 
                                    ym >= (as.yearmon(paste0("Jul", year(as.yearmon(y))))-36/12) &
                                    pf.size=="Big") %>% # Filtering only for big stocks
    arrange(ym,-MV) %>% select(-pf.size,-MV) # Still not selecting the first 1000 by market cap
 
 Cov_spread <- Cov_prep %>%  spread(Id, Excess.RET) %>% select(-ym)
 
 # We require the stock to have 36 returns in the timeframe of 36 months,
 # so if a column has at least one NA for this timeframe, we drop it
 Cov_spread <- Filter(function(x)!any(is.na(x)), Cov_spread)
 
 #Cov_spread <- Cov_spread[, apply(Cov_spread, 2, function(x) !any(is.na(x)))]
 if (!is_empty(Cov_spread)){
 cov_m <- cov(Cov_spread, use = "pairwise.complete.obs")
 cov_m <- as.data.frame(cov_m)
 
 write.csv(cov_m, file = file.path("Min_Var",paste0("cov_",y,".csv")))
 }
}
rm(y)


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
    
    Weight <- read_csv(file.path("Min_Var",paste0("weights_", y,".csv")))
    Weight <- Weight %>% rename("Id" = Row) %>% mutate(Id = as.character(Id))
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
      summarise(Ret = prod(RET)) %>% mutate(Ret = Ret-1)
    portfolio <- left_join(Weight, stocks_ret, by="Id")
    #portfolio_ret <- portfolio %>% 
    portfolio_ret <- weighted.mean(x = portfolio$Ret,w = portfolio$x_vect, na.rm = T)
    portfolio_ret <- as.data.frame(cbind(y, portfolio_ret))
    Portfolio_Returns <- as.data.table(rbind(Portfolio_Returns, portfolio_ret))
      
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}
# Calculate portfolio return
# Check Calculation ...
# Portfolio_Returns <- Portfolio_Returns %>% arrange(y) %>% 
#   mutate(ret = 1+portfolio_ret, Value=100) %>% mutate(cumproduct = cumprod(ret)) %>% mutate(Value = lag(Value)*lag(cumproduct))
# Portfolio_Returns$Value[1] <- 100

Portfolio_Returns <- Portfolio_Returns %>% arrange(y) %>%
  mutate(ret = 1+portfolio_ret) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Portfolio_Returns$Portfolio_Value[1] <- 100

# Calculate Market Portfolio Return for the same time-frane
source("FAT_RM.R")
Market_Portfolio_FAT <- as.data.table(Market_Portfolio_FAT)
Market_Portfolio_FAT[,y:=year(ym)]
Market_Portfolio_FAT[,m := month(ym)]
Market_Portfolio_FAT <- Market_Portfolio_FAT %>% filter(ym>="Jul 1995")
Market_Portfolio_FAT[, hcjun := ifelse(m>=7, y, y-1)]

Market_Ret <- Market_Portfolio_FAT %>% mutate(RM = (RM/100+1)) %>% 
  group_by(hcjun) %>% summarise(Market_Ret = prod(RM)) %>% 
  mutate(Market_Ret = Market_Ret-1) %>% rename("y" = hcjun)
Market_Ret <- Market_Ret %>% arrange(y) %>% 
  mutate(ret = 1+Market_Ret) %>% mutate(Market_Portfolio = 100*lag(cumprod(ret)))
Market_Ret$Market_Portfolio[1] <- 100
Market_Ret <- Market_Ret %>% select(y, Value_Market)
Portfolio_Returns <- left_join(Portfolio_Returns,
                               Market_Ret,
                               by = "y")


# Plot Minimum Variance against the Market Return
plot(x = Portfolio_Returns$y, y = Portfolio_Returns$Portfolio_Value, type = "l", lty=1, ylim=c(50,17000))
lines(x = Portfolio_Returns$y, y = Portfolio_Returns$Market_Portfolio, lty=2)
