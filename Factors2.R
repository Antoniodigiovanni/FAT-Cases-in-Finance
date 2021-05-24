library(data.table)
library(zoo)
library(rstudioapi)
library(tidyverse)
library(dplyr)

# Set the working directory to the script directory
setwd (dirname(getActiveDocumentContext()$path))   

# Set Date Language in English
Sys.setlocale("LC_TIME", "C")

load("FAT_yearly.RData")

Reg.estimates <- FAT.yearly

#BookEquity = common equity + deferred taxes
Reg.estimates$BookEquity <- Reg.estimates$WC03501 + Reg.estimates$WC03263
#Earnings to Price (E/P) = earnings before extraordinary items to Price
Reg.estimates$EtoP <- Reg.estimates$WC01551/ Reg.estimates$PCH.USD
#Cash Flow is operat. CF
Reg.estimates$CF <- Reg.estimates$WC04860
#ROE = earnings before extraordinary items / Book Equity
Reg.estimates$ROE <- Reg.estimates$WC01551/ Reg.estimates$BookEquity
#ROA = earnings before extraordinary items / total assets
Reg.estimates$ROA <- Reg.estimates$WC01551 / Reg.estimates$WC02999
#GP/A = (Net Sales Revenue - COGS)/total Assets
Reg.estimates$GPtoA <- (Reg.estimates$WC01001 -Reg.estimates$WC01051)/Reg.estimates$WC02999
#Operating Profits to book equity = Net Sales Rev. - COGS - Interest - Secondaries)/BookEquity
Reg.estimates$OpPtoE <- (Reg.estimates$WC01001 - Reg.estimates$WC01051 - Reg.estimates$WC01101 - Reg.estimates$WC01251)/Reg.estimates$BookEquity
#Change Operating WC = Change current Assets - change cash (equi) - current liabilities + change current liabilities + change income taxes payable
#Reg.estimates$DeltaOpWC <- Reg.estimates$WC0221 - Reg.estimates$WC02001 -Reg.estimates$WC03101 + Reg.estimates$WC03051 + Reg.estimates$WC03063

Reg.Data <- select(Reg.estimates, Id, country, ICBSUC, YEAR, EtoP, BookEquity, CF, ROE, ROA, GPtoA, OpPtoE) 

Reg.Data <- merge(Reg.Data, FAT.monthly, by.x=c("Id", "YEAR"), by.y=c("Id", "year"))

summary(lm( ~ RMRF.local + SMB + WML)





##Grouping Required
#Operating Accruals = Change Operating WC - depletion - total Assets, in Year y are measured via End fiscal Year Y-2 until EoFY Y-1
OA <- DeltaOpWC - WC01551-WC02999

#Operating Assets = Total Asstes - Cash (Equi)
OA = WC02999 - WC02001
#operating liabilities = total Assets - short&long term Debt - minority interest - prefferd stock&common equity
OL = WC02999 - WC03255 - WC03426 - WC03995

## Grouping Required
#Net Operating Assets = (Operating Assets - Operating Liabilities, both End of fiscal Year Y-1) - total assets and EoFY Y-2
NOA = OA - OL-WC02999


###From Guest Lecture
#Trailing Earnings Yield = Earnings per Share / Price, over Fiscal Year, EPS WC05210
TEY <- EPS/PCH.USD

#Def from guest lecture: Free Cash Flow = Net Income (WC07250)+ DDA (WC04051)+ Non-Cash items + Delta WC (WC04900)+ (Interest -capitalized interest)*(1-tax) - CapEx
#Free Cash Flow per share = WC05507, Common shares outstanding= WC05301
#FCF <- WC05507 * WC05301

#Free Cash Flow Yield (to the Firm)=  FCF/EV
Reg.estimates$EV <- Reg.estimates$BookEquity + Reg.estimates$NetDebt
Reg.estimates$NetDebt <- Reg.estimates$WC03255 - Reg.estimates$WC02001 - Reg.estimates$WC02004 - Reg.estimates$WC02003
Reg.estimates$FCFY <- Reg.estimates$FCF/EV