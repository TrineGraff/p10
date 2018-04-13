source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(glmnet)


lambda = read.csv("results/lasso_lambda.csv") %>% .$x
coef = read.csv("results/lasso_coef.csv") %>% .$row
x = scale(data[ , (colnames(data) %in% coef)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 


forecast = function(y, x, idx = idx, lambda = lambda) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)]) - 1) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = glmnet(x_kov, y_res, alpha = 1, intercept = FALSE)
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    fc[k+1] = x[dim(x_kov)[1], ] %*% beta_hat #finder række i vores df og ganger på beta_hat
    test = length(x[dim(x_kov)[1],]) 
  }
  print(list("fc" = fc, "test" = test))
}

fit = forecast(y, x, idx, lambda)

plot(fit$fc, type = "l", col = "blue", ylim = c(-0.5, 0.5), xlim = c(0, 140))
par(new = TRUE)
plot(y[-c(1:idx)], type = "l", ylim = c(-0.5, 0.5), xlim = c(0, 140))

(mean(se(y[-c(1:idx)] , fit$fc)))
