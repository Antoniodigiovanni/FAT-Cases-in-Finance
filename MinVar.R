library(rstudioapi)


# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")
source("FAT_RM.R")

# Reconstruction date -> July of each year
# We select the first N=1000 stocks for each year and calculate the covariance (less than 1000 present)
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
# Perform optimization on Matlab, in ./Min_Var/Min_Var.m


# Afterwards load the csv files with the optimal weights.
for (y in Years_list){

  # Loading Portfolio Weights
  tryCatch({
    # Create one df for each year with the following line
    #assign(paste0("Weight_",y),read_csv(file.path("Min_Var",paste0("weights_", y,".csv"))))
    
    Weight <- read_csv(file.path("Min_Var",paste0("weights_", y,".csv")))
    print(y)
    print(nrow(Weight))
    print("\n")
  
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
# Perform calculations on portfolios or create some df with all the Ids and the
# weights for each year ...


}
