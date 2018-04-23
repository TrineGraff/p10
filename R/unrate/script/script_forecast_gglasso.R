library(tidyverse)
library(glmnet)
library(gglasso)
source("../data/setup_data.R")

drops = c("UNRATE")
x = scale(data[ , !(colnames(data) %in% drops)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 

forecast_gglasso = function(y, x, grp, lambda) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)])) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = gglasso(x_kov, y_res, grp)
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    fc[k+1] = x[dim(x_kov)[1], ] %*% beta_hat #anvender sidste række i vores design matrice og ganger på beta_hat
  }
  print(fc)
}

