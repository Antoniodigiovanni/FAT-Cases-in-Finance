library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
#create list with all stocks in last month

#for i in 2000:2018{}
tick_2018<-all_data[year <= i & month == 7 &!is.na(RET.USD)]%>% select (Id, RET)
tick <- merge(tick_2018[,c("Id")], all_data[,c("Id", "Date", "RET")], by.x = c("Id"), by.y = c("Id"),
              all.x = T)

log_ret_tidy <- tick %>%select(Id, Date, RET)
log_ret_spread <- log_ret_tidy %>%
  spread(Id, value = RET)
log_ret_spread_1<-log_ret_spread%>%select(-c("Date"))
cov_mat <- cov(log_ret_spread_1/100, use= "pairwise.complete.obs")

nsim <-10000

# Creating a matrix to store the weights

all_wts <- as.data.frame(matrix(nrow = nsim,
                  ncol = nrow(tick_2018)))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = nsim)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = nsim)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = nsim)

for (i in 1:nsim) {
  # Calculate the random weights
  wts <- as.data.frame(runif(n = nrow(tick_2018), min=0, max = 0.2))
  wts <-t(as.data.frame((wts/sum(wts))))
  all_wts[i,]<-wts
  
  
  # Calculate the portfolio returns
  port_ret <- (sum(wts * tick_2018$RET))
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  # Calculate risk
  port_r <- sqrt((wts) %*% (cov_mat %*% t(wts)))
  port_risk[i] <- port_r
  
  # Calculate the Sharpe Ratio
  sr <- port_ret/100/port_r
  
  #store SR
  sharpe_ratio[i] <-sr

}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)
colnames(all_wts)<-colnames(log_ret_spread_1)

portfolio_values <- as.data.frame(cbind(portfolio_values, all_wts))



### use optimizer
require("optimx")
weights<-as.numeric(as_vector(t(wts)))
x<-as.vector(paste(c("x"),1:nrow(tick_2018),sep =""))
f <- function (x) {
 -(if(x>1){-100} else if(x<0){-100}else if(sum(x)!=1){-100}else{as.numeric(sum(x * tick_2018$RET)/(sqrt(t(x) %*% (cov_mat %*% x))))})
}
output<-optimx(par=weights, fn =f,gr=NULL , hess=NULL, lower=0,
               upper=1, method='L-BFGS-B', itnmax=2)
w<-t(as.data.frame(output))
