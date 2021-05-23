
#BookEquity = common equity + deferred taxes
BookEquity <- WC03501 + WC03263
#Earnings to Price (E/P) = earnings before extraordinary items to Price
EtoP <- WC01551/ PCH.USD
#Cash Flow is operat. CF
CF <- WC04860
#ROE = earnings before extraordinary items / Book Equity
ROE <- WC01551/ BookEquity
#ROA = earnings before extraordinary items / total assets
ROA <- WC01551 / WC02999
#GP/A = (Net Sales Revenue - COGS)/total Assets
GPtoA <- (WC01001 - WC01051)/WC02999
#Operating Profits to book equity = /Net Sales Rev. - COGS - Interest - Secondaries)/BookEquity
OpPtoE <- (WC01001 - WC01051 - WC01101 - WC01251)/BookEquity
#Change Operating WC = Change current Assets - change cash (equi) - current liabilities + change current liabilities + change income taxes payable
DeltaOpWC <- WC0221 - WC02001 -WC03101 + WC03051 + WC03063

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
EV <- BookEquity + NetDebt
NetDebt <- WC03255 - WC02001 - WC02004 - WC02003
FCFY <- FCF/EV


# Momentum
all_data[,hcjun := ifelse(month>=7,year,year-1)]


  
