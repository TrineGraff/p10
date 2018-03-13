library(foreach)
library(tidyverse)
library(ggplot2)library(ggplot2)
library(gridExtra)
library(gridExtra)

date <- read.csv("/Users/LouiseNygaardChristensen/Desktop/P10/R/data/FRED_MD/current.csv", sep = ",",
                 header = TRUE)[-1, 1]
rawdata <- read.csv("/Users/LouiseNygaardChristensen/Desktop/P10/R/data/FRED_MD/current.csv", sep = ",",
                    header = TRUE)[-1, -1]
tcodes <- as.numeric(read.csv("/Users/LouiseNygaardChristensen/Desktop/P10/R/data/FRED_MD/current.csv", sep = ",",
                   header = TRUE)[1, -1])


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

transf_data <- foreach(i = 1:ncol(rawdata), .combine = cbind) %do% {
  if(nas[tcodes[i]] != 0) {
    c(rep(NA, nas[tcodes[i]]), transxf(rawdata[, i], tcodes[i]))
  }
  else transxf(rawdata[, i], tcodes[i])
}

colnames(transf_data) <- names(rawdata)
transformed_data <- data.frame(transf_data)
transformed_data 

# NA fjernes --------------------------------------------------------------

n = nrow(transformed_data)
p = ncol(transformed_data)
data_matrix = matrix(nrow = n, ncol = p)

for(j in 1:p){
  for(i in 1:n){
    if(is.na(transformed_data[i,j]))
      data_matrix[i,j] = 1
    else
      data_matrix[i,j] = 0
  }
}

plot(c(1), xlim = c(1,128), ylim = c(1, 708), col = "white", ylab = 'Tidsobservationer', xlab = "Forklarende variable")

for(i in 1:n){
  for(j in 1:p){
    if(data_matrix[i,j] == 1)
      points(j,i, col = "black", pch = 20, cex = 0.1)
  }
}

abline(h = 12, col = "red", lty = "dashed")
abline(h = 704, col = "red", lty = "dashed")

LocateNAs = function(data){
  for(i in 1:ncol(data)){
    number = length(which(is.na(data[ ,i])))
    cat("Kolonne =", i, "\t Antal NAs =", number, "\n")
  }
}
LocateNAs(transformed_data)

# vi vælger at fjerne følgende kolonner: 58, 60, 95, 123, 128
# som svarer til variablene ACOGNO, ANDENOx, TWEXMMTH, UMCSENTx, VXOCLSx
# Yderligere fjernes de første 12 obs samt de sidste 4 obs (pga transformationerne)
# s.a. data starter fra d. 1 jan 1960 til 1 juli 2017
# Vi får altså et datasæt med 691 obs og 124 variable

data <- transf_data[-c(1:12, 704:708), -c(58, 60, 95, 123, 128)]
LocateNAs(data)
dato <- as.Date(date[-c(1:12, 704:708)], format = "%m/%d/%Y")

# ikke-stationær
data_nons <- data.frame(dato, rawdata[-c(1:12, 704:708), -c(58, 60, 95, 123, 128)])

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
data_s <- data.frame(dato, data)
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



