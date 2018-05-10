source("data_unrate.R")
library(matrixcalc)
library(tidyverse)
library(foreach)

#x_f er de forklarende matricer uden anvendelse af skalering
x.f = as.matrix(x_f)

# Faktorerne --------------------------------------------------------------
getFactors <- function(X, r) {
  p = ncol(X) 
  XTX = crossprod(X)
  
  X.eig = eigen(XTX, symmetric = TRUE)
  eig.vec = X.eig$vectors

  loadings = eig.vec[, 1:r] * sqrt(p)
  factors = (X %*% loadings) / p
  
  list.out <- list(
    "factors" = factors ,
    "loading" = loadings
  )
  return(list.out)
}

estFactors <- function(X.df, ic = 1, trace = FALSE) {
  X = as.matrix(X.df)
  n.obs = nrow(X)
  p = ncol(X)
  
  r.max = 20
  ics = rep(NA, r.max)
  
  for (r in 1:r.max) {
    est.r <- getFactors(X, r)
    F.r <- est.r$factors
    L.r <- est.r$loading
    
    if(ic == 1) {
      penalty <- r * (p + n.obs) / (p * n.obs) * log((p * n.obs) / p + n.obs)
    } else if (ic == 2) {
      penalty <- r * (p + n.obs) / (p * n.obs) * log(min(p, n.obs))
    } else if (ic == 3) {
      penalty <- r * log(min(p, n.obs)) / min(p, n.obs)
    } else {
      stop("Invalid information criterion argument")
    }
    
    V.r = matrix.trace(crossprod(X - tcrossprod(F.r, L.r))) / (n.obs * p)
    ics[r] = log(V.r) + penalty
    if (trace) cat("r =", r, "\tV =", V.r, "\tIC =", ics[r], "\n")
  }
  
  r.opt = which.min(ics)
  est.opt = getFactors(X, r.opt)
  factors.opt = est.opt$factors
  colnames(factors.opt) = paste("F", 1:r.opt, sep = "")
  df.out = data.frame(factors.opt) %>% tbl_df
  return(df.out)
}

factors.IC.1 = estFactors(x.f, ic = 1, trace = TRUE)
factors.IC.2 = estFactors(x.f, ic = 2, trace = TRUE)
factors.IC.3 = estFactors(x.f, ic = 3, trace = TRUE)

# Design matrice ----------------------------------------------------------
## Vores nye designmatrice består af faktorerne og m laggede værdier af responsvariablen,
## dvs Z = (F, omega). 
## Vi har en designmatrice for hver IC
## Vi tror m = 1, ..., r ? 

## Design matricen IC 1
r.IC.1 = dim(factors.IC.1)[2] 
df.lags.IC.1 = foreach(i = 1:r.IC.1, .combine = cbind) %do%{
  lag(y, i) 
}
colnames(df.lags.IC.1) = c("lag1", "lag2", "lag3", "lag4", "lag5",
                        "lag6")

Z.IC.1 = data.frame(factors.IC.1, df.lags.IC.1) %>% tbl_df

## Design matricen IC_2
r.IC.2 = dim(factors.IC.2)[2] 
df.lags.IC.2 = foreach(i = 1:r.IC.2, .combine = cbind) %do%{
  lag(y, i) 
}
colnames(df.lags.IC.2) = c("lag1", "lag2", "lag3", "lag4", "lag5",
                        "lag6", "lag7", "lag8", "lag9", "lag10", "lag11")

Z.IC.2 = data.frame(factors.IC.2, df.lags.IC.2) %>% tbl_df

##Design matricen IC_3
r.IC.3 = dim(factors.IC.3)[2] 
df.lags.IC.3 = foreach(i = 1:r.IC.3, .combine = cbind) %do%{
  lag(y, i) 
}
colnames(df.lags.IC.3) = c("lag1", "lag2", "lag3", "lag4", "lag5",
                        "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", 
                        "lag12", "lag13", "lag14", "lag15", "lag16", "lag17",
                        "lag18", "lag19", "lag20")

Z.IC.3 = data.frame(factors.IC.3, df.lags.IC.3) %>% tbl_df

# Forecasting -------------------------------------------------------------
## To måder at behandle vores designmatrice på. 
## Vi sætter enten NA til 0 eller fjerner m rækker i vores designmatrice
## NA's opstår ved tilføjelse af de laggedeværdier i vores designmatrice

# Ser bort fra de første r observationer
factorForecast <- function(Y, Z, r, idx = (idx)) {
  ## Skal kører setup_data
  
  Z = as.matrix(Z)
  n.obs = length(y_test) 

  fc = foreach(i = 1:n.obs, .combine = rbind) %do%{
    #starter fra r + 1, for at undgå NA's. 
    #Så vi ser bort fra de første 6 rækker i vores designmatrice
    Z.in = Z[(r+1):(idx + i - 1), ] 
    Y.res = Y[(r+1):(idx + i - 1)]
    Z.out = Z[idx + i, ]  
    
    ZTZ = crossprod(Z.in)
    ZY = crossprod(Z.in, Y.res)
    beta = solve(ZTZ, ZY)
    
    Y.fc <- Z.out %*% beta
    return(Y.fc)
    
  }
  print(fc)
}

#Sætter NA's til 0
factorForecast_test <- function(Y, Z, r, idx = (idx)) {
  ## Skal kører setup_data
  
  Z = as.matrix(Z)
  n.obs = length(y_test) 
  Z[is.na(Z)] = 0 #sætter NA's til 0
  
  fc = foreach(i = 1:n.obs, .combine = rbind) %do%{
    Z_in = Z[1:(idx + i - 1), ] 
    Y_res = Y[1:(idx + i - 1)]
    Z_out = Z[idx + i, ]  
    
    ZTZ = crossprod(Z_in)
    ZY = crossprod(Z_in, Y_res)
    beta = solve(ZTZ, ZY)
    
    Y_fc <- Z_out %*% beta
    return(Y_fc)
    
  }
  print(fc)
  
}

# Analyse -----------------------------------------------------------------
## Vi bruger MSE til at yndersøge prædiktionsevnen
mse <- function(error)
{
  (mean(error^2))
}

## IC 1
fc.IC.1 = factorForecast(y, Z.IC.1 , r = r.IC.1, idx = idx)
mse(fc.IC.1 - y_test)

##IC 2 
fc.IC.2 = factorForecast(y, Z.IC.2 , r = r.IC.2, idx = idx)
mse(fc.IC.2 - y_test)

## IC 3
fc.IC.3 = factorForecast(y, Z.IC.3 , r = r.IC.3 , idx = idx)
mse(fc.IC.3 - y_test)

##Vælger IC 3, da den har mindst MSE

# plot --------------------------------------------------------------------
dato = c(as.character(test_dato))
df = data.frame(date = as.Date(dato), fc = fc.IC.3, y = y_test)

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y, colour = "Arbejdsløshed")) +
  geom_line(aes(y = fc, colour = "Faktor model")) +
  ylab("Rate") + xlab("Dato") +
  scale_colour_manual(values = c("grey", "red")) +
  theme(legend.title=element_blank()) 


