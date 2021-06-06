################################################################################
#                                                                              #
# Do not run the entire script! It requires a Matlab script before the second  #
# loop can be executed, run until line 54 and then move to Matlab              #
#                                                                              #
################################################################################

library(rstudioapi)
library(tidyverse)
#library(rapportools)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 

source("Real_data_prep.R")
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"))
source("ReturnForecast.R")

# Reconstruction date -> July of each year
# matrix based on the excess returns of the previous 36 months excess returns

# Stocks selection
stocks <- all_data %>% select(Id, country, Date, ym, year, month, RET, MV, pf.size)
stocks <- left_join(stocks,
                    FF[,c("ym","RF")],
                    by="ym")



# Still not ordering by Market Cap (less than 1000 big stocks (with complete obs) each year, maybe we use all of them?)
RET$year <- year(RET$ym)
Years_list <- unique(RET$year)
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

  if (!is_empty(Cov_spread_t) & !is_empty(Ret_t)){
    cov_m <- cov(Cov_spread_t, use = "pairwise.complete.obs")
    cov_m <- as.data.frame(cov_m)
    write.csv(Ret_t, file = file.path("Max_Sharpe", paste0("Ret_",y,".csv")))
    write.csv(cov_m, file = file.path("Max_Sharpe",paste0("cov_",y,".csv")))
  }
  
  
}
rm(y, cov_m, Cov_prep, Cov_spread,Exist_in_July,Investable_Ids)



###########################################################
#                                                         #
# Perform optimization on Matlab, in ./Min_Var/Min_Var.m  #
#                                                         #
###########################################################

# Afterwards load the csv files with the optimal weights.
Portfolio_Returns <- data.table()
Std_Deviation <- data.table()
meanWeights <- as.data.frame(matrix(nrow = length(Years_list)-1,
                                ncol = 2))
NumberOfStock <-as.data.frame(matrix(nrow = length(Years_list)-1,
                                     ncol = 2))
Hit <-as.data.frame(matrix(nrow = length(Years_list)-1,
                                     ncol = 3))

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
    
    
    #for portfolio concentration,Top 10
    top10 <- Weight %>% arrange(desc(x_vect)) %>% slice_head(n = 10)
    
    meanWeights[y-1997,1]<-y
    meanWeights[y-1997,2]<- top10 %>% summarise(mean(x_vect))
    
    #Number of stocks per year in Portfolio
   
    NumberOfStock[y-1997,1]<-y
    hlp <- Weight %>% filter(x_vect>0)
    NumberOfStock[y-1997,2]<-nrow(hlp)
    rm(hlp)
    
    # Perform calculations on portfolios or create some df with all the Ids and the
    # weights for each year ...
    stocks_ret <- stocks %>% 
      filter(ym >= as.yearmon(paste0("Jul", year(as.yearmon(y)))) &
               ym < (as.yearmon(paste0("Jul", year(as.yearmon(y))))+1))
    stocks_temp <- stocks_ret
    stocks_ret <- stocks_ret %>% filter(Id %in% Weight$Id) %>% mutate(RET = (RET/100+1)) %>% 
      group_by(Id) %>% 
      summarise(Ret = prod(RET, na.rm = T)) %>% mutate(Ret = Ret-1)
    
    #Calculate Input for Hit Rate
    Hit[y-1997,1]<-y
    hlp <- Weight %>% filter(x_vect>0)
    Hit[y-1997,2]<-nrow(hlp)
    rm(hlp)
    
    hlp<-stocks_ret %>% filter(Ret>0)
    Hit[y-1997,3]<-nrow(hlp)
    rm(hlp)
    # Calculate annualized mean monthly excess return, return and annualized standard dev.
    
    stocks_temp <- stocks_temp %>% mutate(Excess_RET = (RET-RF)/100)
    stocks_temp <- left_join(stocks_temp, Weight,
                             by="Id")
    Portfolio_monthly_RET <- stocks_temp %>% group_by(ym) %>% 
      summarise(Portfolio_Excess_RET = sum(Excess_RET * x_vect, na.rm = T),
                Port_RET = sum(RET/100 * x_vect, na.rm = T))
    
    # Annualized_Return <- stocks_temp %>% 
    #   summarise(Mean = mean(Excess_RET, na.rm = T)) %>% mutate(Mean = (1+Mean)^12,
    #                                                            Mean = Mean -1)
                                          
    Std_Dev <- Portfolio_monthly_RET %>% summarise(Std = sd(Port_RET)*sqrt(12))
    
    portfolio <- left_join(Weight, stocks_ret, by="Id")
    portfolio <- portfolio %>% arrange(-x_vect)
    
    portfolio_ret <- sum(portfolio$Ret*portfolio$x_vect, na.rm=TRUE)
    portfolio_ret <- as.data.frame(cbind(y, portfolio_ret))
    Portfolio_Returns <- as.data.table(rbind(Portfolio_Returns, portfolio_ret))
    
    Std_Dev <- as.data.frame(cbind(y, Std_Dev))
    Std_Deviation <- as.data.table(rbind(Std_Deviation, Std_Dev))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

Portfolio_Returns_SR <- Portfolio_Returns %>% arrange(y) %>%
  mutate(ret = 1+portfolio_ret) %>% mutate(Portfolio_Value = 100*lag(cumprod(ret)))
Portfolio_Returns_SR$Portfolio_Value[1] <- 100
#write_csv(Portfolio_Returns,"Portfolio_Returns_SR.csv")

##Calculate the Sharpe Ratio using the mean of the yearly Sharpe Ratio, i.e. the mean of annual returns divided by the annual std.dev.
FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF)) 
SR<-Portfolio_Returns_SR
SR<-merge(SR, FF, by.x=c("y"), by.y=c("Year"))
SR <- left_join(SR, Std_Deviation, by="y") %>% mutate(YRF = YRF - 1)
SR <- SR %>% mutate(Sharpe_Ratio = (portfolio_ret-YRF)/Std)
SharpeRatio <- as.data.frame(mean(SR$Sharpe_Ratio))
names(SharpeRatio)[names(SharpeRatio)=="mean(SR$Sharpe_Ratio)"]<-c("SharpeRatio")

##Volatility
Volatility <- as.data.frame(sd(SR$portfolio_ret))
names(Volatility)[names(Volatility)=="sd(SR$portfolio_ret)"] <- c("Volatility")

#Annualized Return
AR<-as.data.frame(mean(SR$portfolio_ret)*100)
colnames(AR)[1]<-"Annualized Return"

#Concentration
Concentration <- as.data.frame(mean(meanWeights$V2))
names(Concentration)[names(Concentration)=="mean(meanWeights$V2)"] <- c("Concentration")

#years <- 2018-1995+1
#Volatility2 <- mean(Std_Deviation$Std)*sqrt(years)

#Cumulative Risk Free rate return
# SR <- SR %>% arrange(y) %>%
#   mutate(ret = 1+YREF) %>% mutate(RF_Value = 100*lag(cumprod(ret)))
# Portfolio_Returns_SR$Portfolio_Value[1] <- 100

# Sharpe Ratio calculated annualizing the cumulative return and risk free rate

# SR <- SR%>% mutate(TRF = 100*lag(cumprod(YRF)))
# SR$TRF[1] <- 100
# CumRF <- tail(SR$TRF,n=1)/100
# CumPR <- tail(SR$Portfolio_Value,n=1)/100
# Vol <- sd(SR$portfolio_ret)
# SharpeRatio <- (CumPR-CumRF)/sd(SR$portfolio_ret)
# 
# 
# years <- 2018-1995+1
# CumPR <- CumPR^(1/years)
# CumRF <- CumRF^(1/years)
#Vol <- mean(SR$Std)*sqrt(years)
# SharpeRatio <- (CumPR-CumRF)/Vol

Results_SR <- merge(Volatility, Concentration)
Results_SR <- merge(Results_SR, SharpeRatio)
Results_SR <- merge(Results_SR, AR)


###Creating the Weight df
Years_list <- read_csv(file.path("Max_Sharpe", "Years_list.csv")) %>% 
  select(x) %>% arrange(x) %>% 
  as.list() %>% unlist()
Weights_df <- data.table()
j <- 0
for (y in Years_list){
  tryCatch({
    # Create one df for each year with the following line
    #assign(paste0("Weight_",y),read_csv(file.path("Min_Var",paste0("weights_", y,".csv"))))
    print(y)
    Weight <- read_csv(file.path("Max_Sharpe",paste0("weights_", y,".csv")))
    # Normalizing weights (as weights of 10^-5 and lower have been set to 0)
    Weight <- Weight %>% rename("Id" = Row) %>% 
      mutate(Id = as.character(Id),
             x_vect = x_vect/sum(x_vect)) %>% 
      arrange(-x_vect)
    
    if (j == 0) {
      Weights_df <- Weight
      names(Weights_df)[names(Weights_df) == "x_vect"] <- y
    } else {
      Weights_df <- full_join(Weights_df, Weight, by="Id")
      names(Weights_df)[names(Weights_df) == "x_vect"] <- y
    }
    
    j = j+1 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Portfolio Figures calculation
Weights_df[is.na(Weights_df)] <- 0

# Yearly turnover (as per Hanauer, Lauterbach (2019))
T <- ncol(Weights_df) - 1 #1 column per year + 1 for the Ids
transposed_Weights <- as.data.frame(t(Weights_df))
names(transposed_Weights) <- Weights_df$Id
transposed_Weights <- transposed_Weights[-1,]
to <- function(weight){
  result <- abs(as.numeric(lead(weight)) - as.numeric(weight))
}
Turnover_df <- transposed_Weights %>% mutate(across( .fns = to)) %>% drop_na()
Turnover <- as.data.frame(sum(Turnover_df)/(2*T))
colnames(Turnover)[1]<-"Turnover"
Results_SR <- merge(Results_SR, Turnover)

# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- (as.numeric(weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- as.data.frame(Effective_N/T)
colnames(Effective_N)[1]<-"Effective_N"
Results_SR <- merge(Results_SR, Effective_N)


#Max Drawdown
MD <- as.data.frame(-min(Portfolio_Returns_SR$portfolio_ret)*100)
colnames(MD)[1] <- "MD"
Results_SR <- merge(Results_SR, MD)

#Total Cummultated Return
TCR <- as.data.frame(tail(Portfolio_Returns_SR$Portfolio_Value,n=1))
colnames(TCR)[1]<-"Total Cum. Return"
Results_SR <- merge(Results_SR, TCR)

#Avg Number of Stocks
avgStocks <- as.data.frame(mean(NumberOfStock$V2))
colnames(avgStocks)[1]<-"avg. Number of stocks"
Results_SR <- merge(Results_SR, avgStocks)

#Information Ratio
source("MSCI.R")
MP <- MSCI_weighted %>% select(c("year", "ret"))
colnames(MP)[2]<-"YMR"
IR<-Portfolio_Returns_SR
IR<-merge(IR, MP, by.x=c("y"), by.y=c("year"))
IR <- left_join(IR, Std_Deviation, by="y") %>% mutate(YMR = YMR - 1)
IR <- IR %>% mutate(InfRatio= (portfolio_ret-YMR)/Std)
InfRatio <- as.data.frame(mean(IR$InfRatio))
colnames(InfRatio)[1]<-"Information Ratio"
Results_SR <- merge(Results_SR, InfRatio)

#Tracking Error
retPeriods <- 2018-1995+1-1
IR$help <-IR$portfolio_ret-IR$YMR
TE <- as.data.frame(sqrt(sum((IR$help)^2)/retPeriods)*100)
colnames(TE)[1]<-"TrackingError"
Results_SR <- merge(Results_SR, TE)

#HitRate
colnames(Hit)[2]<-"totalNumberOfStocks"
colnames(Hit)[3]<-"posStocks"
Hit<-Hit%>% mutate(HitRate = posStocks/totalNumberOfStocks)
HR<-as.data.frame(mean(Hit$HitRate)*100)
colnames(HR)[1]<-"HitRate"
Results_SR <- merge(Results_SR, HR)                          





rownames(Results_SR)[1]<-"SRM-Portfolio"
#rm(y,Weight,top10,stocks_ret,stocks_temp,Std_Dev,Std_Deviation,SR,SharpeRatio,Portfolio_monthly_RET,Portfolio_Returns, portfolio, meanWeights,FF,Volatility, Concentration,stocks)