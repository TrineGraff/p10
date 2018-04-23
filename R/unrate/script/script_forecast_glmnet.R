library(tidyverse)
library(glmnet)
source("../data/setup_data.R")

drops = c("UNRATE")
x = scale(data[ , !(colnames(data) %in% drops)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 


forecast_glmnet = function(y, x, idx = idx, lambda, alpha, penalty.factor = NULL) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)])) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = glmnet(x_kov, y_res, alpha = alpha, intercept = FALSE, penalty.factor )
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    fc[k+1] = x[dim(x_kov)[1], ] %*% beta_hat #anvender sidste række i vores design matrice og ganger på beta_hat
  }
  print(fc)
}
