lambda = read.csv("results/ridge_lambda.csv") %>% .[1, 2]

source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(glmnet)

drops = c("UNRATE")
x = scale(data[ , !(colnames(data) %in% drops)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 
# ridge -------------------------------------------------------------------

forecast = function(y, x, idx = idx, lambda) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)])) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = glmnet(x_kov, y_res, alpha = 0, intercept = FALSE)
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    #starter med at estimerer fc[idx]
    fc[k+1] = x[dim(x_kov)[1], ] %*% beta_hat #finder række i vores df og ganger på beta_hat
  }
  print(list("fc" = fc))
}

fit_ridge = forecast(y, x, idx, lambda) 

plot(fit_ridge$fc, type = "l", col = "red", ylim = c(-0.5, 0.5), xlim = c(0, 140))
par(new = TRUE)
plot(c(y[idx], y[-c(1:idx)]), type = "l", ylim = c(-0.5, 0.5), xlim = c(0, 140))


loss = function(fc, y_test){
  print(mae(y_test, fc))
  print(rmse(y_test, fc))
}

loss(fit$fc, y[-c(1:idx)]) #0.1590, 0.20
