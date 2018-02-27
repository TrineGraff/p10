library(ggplot2)
library(gridExtra)
library(foreach)
library(tidyverse)

rawdata <- read.csv("/Users/LouiseNygaardChristensen/Desktop/P10/R/data/FRED_MD/current.csv", sep = ",",
                    header = TRUE)
head(rawdata)
tail(rawdata)

data <- rawdata[-1, -1]

# Behandler data ----------------------------------------------------------

n = nrow(data)
p = ncol(data)
data_matrix = matrix(nrow = n, ncol = p)

for(j in 1:p){
  for(i in 1:n){
    if(is.na(data[i,j]))
      data_matrix[i,j] = 1
    else
      data_matrix[i,j] = 0
  }
}

plot(c(1), xlim = c(1,139), ylim = c(1, 708), col = "white", ylab = 'Tidsobservationer', xlab = "Forklarende variable")

for(i in 1:n){
  for(j in 1:p){
    if(data_matrix[i,j] == 1)
      points(j,i, col = "black", pch = 20, cex = 0.1)
  }
}

abline(h = 13, col = "red", lty = "dashed")
abline(h = 706, col = "red", lty = "dashed")


LocateNAs = function(data){
  for(i in 1:ncol(data)){
    number = length(which(is.na(data[ ,i])))
    cat("Kolonne =", i, "\t Antal NAs =", number, "\n")
  }
}
LocateNAs(data)
# vi vælger at fjerne følgende kolonner: 58, 60, 95, 123, 128
# som svarer til variablene ACOGNO, ANDENOx, TWEXMMTH, UMCSENTx, VXOCLSx

# Yderligere fjernes de først 13 obs samt de sidste 3 obs
# s.a. data starter fra d. 1 feb 1960 til 1 juli 2017
# Vi får altså et datasæt med 691 obs og 123 variable

fjernet_var <- rawdata[c(2:14, 705,709), c(1, 59, 61, 96, 124, 129)]
non.na_data <- rawdata[-c(2:14, 705:709), -c(1, 59, 61, 96, 124, 129)]
LocateNAs(non.na_data)


# Transformation af data --------------------------------------------------

transxf <- function(x, tcode) {
  if(tcode == 1) x
  else if(tcode == 2) diff(x)
  else if(tcode == 3) diff(x, 2)
  else if(tcode == 4) log(x)
  else if(tcode == 5) diff(log(x))
  else if(tcode == 6) diff(log(x), 2)
  else if(tcode == 7) diff(x[-1] / x[-length(x)] - 1)
}

nas <- c(0, 1, 2, 0, 1, 2, 2)
tcodes <- as.numeric(non.na_data[1, ])

transf_data <- foreach(i = 1:ncol(non.na_data), .combine = cbind) %do% {
  if(nas[tcodes[i]] != 0) {
    c(rep(NA, nas[tcodes[i]]), transxf(non.na_data[-1, i], tcodes[i]))
  }
  else transxf(non.na_data[-1, i], tcodes[i])
}
colnames(transf_data) <- names(non.na_data)
transformed_data <- data.frame(transf_data)


# target variable ---------------------------------------------------------

dato <- as.Date(rawdata$sasdate[-c(1:14, 705:709)], format = "%m/%d/%Y")

# ikke-stationær
data_nons <- data.frame(dato, 
  rawdata[-c(1:14, 705:709), -c(1, 59, 61, 96, 124, 129)])

rpi_nons <- ggplot(data_nons, aes(dato, RPI)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("RPI")
unrate_nons <- ggplot(data_nons, aes(dato, UNRATE)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("UNRATE")
m1sl_nons <- ggplot(data_nons, aes(dato, M1SL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("M1SL")
m2sl_nons <- ggplot(data_nons, aes(dato, M2SL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("M2SL")
fedfunds_nons <- ggplot(data_nons, aes(dato, FEDFUNDS)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("FEDFUNDS")
tb3ms_nons <- ggplot(data_nons, aes(dato, TB3MS)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("TB3MS")
gs5_nons <- ggplot(data_nons, aes(dato, GS5)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("GS5")
exjpusx_nons <- ggplot(data_nons, aes(dato, EXJPUSx)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("EXJPUSx")
cpiaucsl_nons <- ggplot(data_nons, aes(dato, CPIAUCSL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("CPIAUCSL")
sp500_nons <- ggplot(data_nons, aes(dato, S.P.500)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("S.P.500")

grid.arrange(rpi_nons, unrate_nons, m1sl_nons, m2sl_nons, fedfunds_nons, 
             tb3ms_nons, gs5_nons, exjpusx_nons, cpiaucsl_nons, sp500_nons, ncol=2) 

# stationær
data_s <- data.frame(dato, transformed_data)
write.csv(data_s, file = "transformed_data.csv")

rpi_s <- ggplot(data_s, aes(dato, RPI)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("RPI")
unrate_s <- ggplot(data_s, aes(dato, UNRATE)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("UNRATE")
m1sl_s <- ggplot(data_s, aes(dato, M1SL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("M1SL")
m2sl_s <- ggplot(data_s, aes(dato, M2SL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("M2SL")
fedfunds_s <- ggplot(data_s, aes(dato, FEDFUNDS)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("FEDFUNDS")
tb3ms_s <- ggplot(data_s, aes(dato, TB3MS)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("TB3MS")
gs5_s <- ggplot(data_s, aes(dato, GS5)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("GS5")
exjpusx_s <- ggplot(data_s, aes(dato, EXJPUSx)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("EXJPUSx")
cpiaucsl_s <- ggplot(data_s, aes(dato, CPIAUCSL)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("CPIAUCSL")
sp500_s <- ggplot(data_s, aes(dato, S.P.500)) + geom_line() +
  labs(x = " ", y = "?") + ggtitle("S.P.500")

grid.arrange(rpi_s, unrate_s, m1sl_s, m2sl_s, fedfunds_s, 
             tb3ms_s, gs5_s, exjpusx_s, cpiaucsl_s, sp500_s, ncol=2) 



# Opdeling af data --------------------------------------------------------
data <- transformed_data

group1 <- data.frame(data$RPI, data$W875RX1, data$INDPRO, data$IPFPNSS, data$IPFINAL, data$IPCONGD,
                data$IPDCONGD, data$IPNCONGD, data$IPBUSEQ, data$IPMAT, data$IPDMAT, data$IPNMAT,
                data$IPMANSICS, data$IPB51222S, data$IPFUELS, data$CUMFNS)

group2 <- data.frame(data$HWI, data$HWIURATIO, data$CLF16OV, data$CE16OV, data$UNRATE, data$UEMPMEAN,
                data$UEMPLT5, data$UEMP5TO14, data$UEMP15OV, data$UEMP15T26, data$UEMP27OV, data$CLAIMSx,
                data$PAYEMS, data$USGOOD, data$CES1021000001, data$USCONS, data$MANEMP, data$DMANEMP,
                data$NDMANEMP, data$SRVPRD, data$USTPU, data$USWTRADE, data$USTRADE, data$USFIRE, 
                data$USGOVT, data$CES0600000007, data$AWOTMAN, data$AWHMAN, data$CES0600000008, data$CES2000000008,
                data$CES3000000008)

group3 <- data.frame(data$HOUST, data$HOUSTNE, data$HOUSTMW, data$HOUSTS, data$HOUSTW, data$PERMIT,
                data$PERMITNE, data$PERMITMW, data$PERMITS, data$PERMITW)

group4 <- data.frame(data$DPCERA3M086SBEA, data$CMRMTSPLx, data$RETAILx, data$AMDMNOx,
                data$AMDMUOx, data$BUSINVx, data$ISRATIOx)

group5 <- data.frame(data$M1SL, data$M2SL, data$M2REAL, data$AMBSL, data$TOTRESNS, data$NONBORRES,
                data$BUSLOANS, data$REALLN, data$NONREVSL, data$CONSPI, data$MZMSL, data$DTCOLNVHFNM,
                data$DTCTHFNM, data$INVEST)

group6 <- data.frame(data$FEDFUNDS, data$CP3Mx, data$TB3MS, data$TB6MS, data$GS1, data$GS5,
                data$GS10, data$AAA, data$BAA, data$COMPAPFFx, data$TB3SMFFM, data$TB6SMFFM,
                data$T1YFFM, data$T5YFFM, data$T10YFFM, data$AAAFFM, data$BAAFFM, data$EXSZUSx, 
                data$EXJPUSx, data$EXUSUKx, data$EXCAUSx)

group7 <- data.frame(data$WPSFD49207, data$WPSFD49502, data$WPSID61, data$WPSID62, data$OILPRICEx, data$PPICMM,
                data$CPIAUCSL, data$CPIAPPSL, data$CPITRNSL, data$CPIMEDSL, data$CUSR0000SAC, data$CUSR0000SAD,
                data$CUSR0000SAS, data$CPIULFSL, data$CUSR0000SA0L2, data$CUSR0000SA0L5,data$PCEPI, data$DDURRG3M086SBEA,
                data$DNDGRG3M086SBEA, data$DSERRG3M086SBEA)

group8 <- data.frame(data$S.P.500, data$S.P..indust, data$S.P.div.yield, data$S.P.PE.ratio)
