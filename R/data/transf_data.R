library(foreach)
library(tidyverse)

FRED_MD <- read.csv("../data/FRED_MD/current.csv", sep = ",", header = TRUE)
date <- as.Date(FRED_MD[-1, 1], format = "%m/%d/%Y")
rawdata <- FRED_MD[-1, -1]
tcodes <- as.numeric(FRED_MD[1, -1])

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

transformed_data[, 58] == transformed_data[, 'ACOGNO']
transformed_data[, 60] == transformed_data[, 'ANDENOx']
transformed_data[, 95] == transformed_data[, 'TWEXMMTH']
transformed_data[, 123] == transformed_data[, 'UMCSENTx']
transformed_data[, 128] == transformed_data[, 'VXOCLSx']

# vi vælger at fjerne følgende kolonner: 58, 60, 95, 123, 128
# som svarer til variablene ACOGNO, ANDENOx, TWEXMMTH, UMCSENTx, VXOCLSx
# det er S.P.PE.ratio som har 5 na til slut (transformed_data$S.P.PE.ratio)
# som naturlig følge af transformationerne, vil vi have max 3 na i starten.
# 5 tidsrækker har 12 na i starten (transformed_data[,53]-transformed_data[,57])

# Derfor fjernes de første 12 obs samt de sidste 5 obs
# s.a. data starter fra d. 1 jan 1960 til 1 juli 2017
# Vi får altså et datasæt med 691 obs og 123 variable

data <- transformed_data[-c(1:12, 704:708), -c(58, 60, 95, 123, 128)]
LocateNAs(data)
dato <- date[-c(1:12, 704:708)]

df <- data.frame(dato, data)
write.csv(df, file = "transformed_data.csv")
