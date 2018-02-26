
library(Quandl)

  GDP = Quandl("FRED/GDP")
## Real Output and Income (ALL SA, and quartly)

  IPS10 = Quandl("FRED/IPB50001SQ")
  IPS11 = 
  IPS299 = Quandl("FRED/IPB50002SQ")
  IPS12 = Quandl("FRED/IPB51000SQ")
  IPS13 = Quandl("FRED/IPB51100SQ")
  IPS18 = Quandl("FRED/IPB51200SQ")
  IPS25 = Quandl("FRED/IPB52100SQ")
  IPS32 = Quandl("FRED/IPB53000SQ")
  IPS34 = Quandl("FRED/IPB53100SQ")
  IPS38 = Quandl("FRED/IPB53200SQ")
  IPS43 = Quandl("FRED/IPB00004SQ")
  IPS307 = Quandl("FRED/IPB51222SQ")
  IPS306 =Quandl("FRED/IPB51221SQ")
  PMP =
  UTL11 = Quandl("FRED/CAPB00004SQ")

## Hourly Earnings


## (ALL) Employment (MONTHLY)
  CES002 = Quandl("FRED/CEU0500000001") #NSA
  CES003 = Quandl("FRED/CEU0600000001") #NSA
  CES006 = Quandl("FRED/CES1021000001") #SA
  CES011 = Quandl("FRED/USCONS") #SA
  CES015 = 
  CES017 = Quandl("FRED/DMANEMP") #SA
  CES033 = Quandl("FRED/NDMANEMP") #SA
  CES046 =
  CESO48 = Quandl("FRED/USTPU") #SA
  CES049 = Quandl("FRED/USWTRADE") #SA
  CES053 = Quandl("FRED/USTRADE") #SA
  CES088 = Quandl("FRED/USFIRE") #SA
  CES140 = Quandl("FRED/USGOVT") #SA
  LHEL = 
  LHELX = 
  LHEM = Quandl("FRED/CLF16OV")
  LHNAG = 
    
  
## Unenployment (MONTLY)
  LHUR = 
  LHU680 = Quandl("FRED/UEMPMEAN") #SA #Weeks
  LHU5 =  Quandl("FRED/LNS13008397") #SA
  LHU14 = Quandl("FRED/LNS13025701") #SA
  LHU15 = Quandl("FRED/M08320USM156SNBR") #SA (målt i A Per Cent Of The Total Civilian Labor Force)
  LHU26 = Quandl("FRED/LNS13025702") #SA
  LHU27 = Quandl("FRED/LNS13025703") #SA
  
##Hours 
  CES151 = 
  CES155 = 
    
## Hours Starts and Sales (MONTLY) (Thousind of units)
  HSBR = Quandl("FRED/HOUST") #Unit SA
  HSFR =
  HSNE =  Quandl("FRED/HOUSTNE") #SA
  HSMW = Quandl("FRED/HOUSTMW") #SA
  HSSOU = Quandl("FRED/HOUSTS") #SA
  HSWST = Quandl("FRED/HOUSTW") #SA
  
##Interest Rates (PROCENT) (NSA) (MONTLY)
FYFF = Quandl("FRED/FEDFUNDS") #NSA
  FYGM3 = Quandl("FRED/TB3MS") #NSA
  FYGM6 =  Quandl("FRED/TB6MS") #NSA
  FYGT1 = Quandl("FRED/GS1") #NSA
  FYGT5 = Quandl("FRED/GS5") #NSA
  FYG10 = Quandl("FRED/GS10") #NSA
  FYAAAC = 
  FYBAAC = 

## Interest Rates Differences 
SFYGM6 = 
  SFYGT1 =
  SFYGT10 =
  SFYAAAC
  SFYBAAC
    
## Money and Credit Quantoty Aggregates
  FM1 = Quandl("FRED/M1SL") #SA
  MZMSL = Quandl("FRED/MZM") #SA
  FM2 = Quandl("FRED/M2SL") #SA
  FMFBA = 
  FMRRA = Quandl("FRED/TOTRESNS") #NSA
  BUSLOANS = Quandl("FRED/BUSLOANS") #SA
  CCINRV = 
    
## Consumptions 
  PI071 =
  PI072 = Quandl("FRED/PCEDG") #SA
  PI073 = Quandl("FRED/PCEND") #SA
  PI074 = Quandl("FRED/PCES") #SA
      
## Price Indices
  CPIAUCSL = 
  CPILFESL =
  PCEPILFE = 
  PWFSA = 
  PWFCSA = 
  PWIMSA = 
  PWCMSA = 
  PWCMSAR =
  PSCCOM =
  PSCOOMR = 
  PW561 =
  PW561R =
  PMCP =
        
        
## Exchange Rates
  EXRUS = Quandl("FRED/EXUSEU") #NSA #
  EXRSW = Quandl("FRED/EXSZUS") #NSA
  EXRJAN = Quandl("FRED/EXJPUS") #NSA
  EXRUK = Quandl("FRED/FXRATEGBA618NUPN") #årligt #NSA
  EXRCAN = Quandl("FRED/EXCAUS") #NSA
        
## Stock Prices and Miscellaneous
  FSPCOM = 
  FSPIN = 
  FSDXP = 
  FDPXE = 
  FSDJ = 
  HHSNTN =

## Inventories and Orders
  PMI = 
  PMNO = 
  PMDEL = 
  PMNV =
  MOCMQ = 
  MSONDQ = 