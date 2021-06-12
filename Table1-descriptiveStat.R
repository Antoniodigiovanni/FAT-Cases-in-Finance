
library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(rlang)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")
source("Real_data_prep.R")

MarketCValues <- all_data %>% group_by(country) %>% summarise(meanSize=mean(MV.USD.June))
MarketCValues2 <- all_data %>% group_by(country, ym) %>% summarise(Ids=length(Id))
MarketCValues2<- MarketCValues2 %>% group_by(country) %>% summarise(Ids=mean(Ids))
MarketCValues3 <- all_data %>% group_by(country, ym) %>% summarise(totSize=sum(MV.USD.June))
MarketCValues3 <-MarketCValues3 %>% filter(ym == "Dec 2018")
hlp<-sum(MarketCValues3$totSize)
MarketCValues3 <- MarketCValues3 %>% mutate(share = totSize/hlp)
rm(hlp)
MarketCValues4 <- all_data %>% group_by(country) %>% summarise(medianSize=median(MV.USD.June))
                                            
                                            