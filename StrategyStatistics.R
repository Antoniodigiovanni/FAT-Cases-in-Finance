library(rstudioapi)
library(tidyverse)
library(data.table)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path)) 


#source("")

# If we have the list of Weights csv files in the folder we can load them
# and create a combined df

Years_list <- read_csv(file.path("Min_Var", "Years_list.csv")) %>% 
  select(x) %>% arrange(x) %>% 
  as.list() %>% unlist()
Weights_df <- data.table()
j <- 0
for (y in Years_list){
  tryCatch({
    # Create one df for each year with the following line
    #assign(paste0("Weight_",y),read_csv(file.path("Min_Var",paste0("weights_", y,".csv"))))
    print(y)
    Weight <- read_csv(file.path("Min_Var",paste0("weights_", y,".csv")))
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
Turnover <- sum(Turnover_df)/(2*T)


# Effective N (as per Hanauer, Lauterbach (2019))
eN <- function(weight){
  result <- (as.numeric(weight)^2)
}
Effective_N_df <- transposed_Weights %>% mutate(across(.fns = eN))
inverse <- function(weight){
  result <- 1/weight
}
Effective_N <- as.data.frame(rowSums(Effective_N_df)) %>% mutate(across( .fns = inverse)) %>% sum()
Effective_N <- Effective_N/T



# Maximum Drawdown

MD <- -min(Portfolio_Returns$ret)



# Sharpe Ratio

FF <- read_csv("FF Monthly.CSV") %>% 
  rename(ym = X1) %>%  
  mutate(ym = as.yearmon(as.character(ym), "%Y%m"),
         Year = year(ym))
FF <- FF %>% group_by(Year) %>% mutate(RF=(RF/100+1)) %>% summarise(YRF=prod(RF)) 
SR<-Portfolio_Returns
SR<-merge(SR, FF, by.x=c("y"), by.y=c("Year"))
SR <- SR%>% mutate(TRF = 100*lag(cumprod(YRF)))
SR$TRF[1] <- 100
CumRF <- tail(SR$TRF,n=1)/100
CumPR <- tail(SR$Portfolio_Value,n=1)/100
Vol <- sd(SR$portfolio_ret)
SharpeRatio <- (CumPR-CumRF)/sd(SR$portfolio_ret)


#Return (Average arithmetic return(annualized) as a percentage)
# SD (standard deviation (annualized))

# Tracking Error (to the value weighted portfolio)

#concentration

