source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(Metrics)

y = scale(data_raw[, "UNRATE"], scale = FALSE)

forecast = function(data, p, h, idx = idx ) {
  fc = c(NA)
  for(k in 0:length(data[-c(1:idx)]) -1) {
    y = data[(p + 1):(length(data[1:idx]) + k)] #y bliver opdateret med den observerede værdi for hvert k
    n.obs = length(y) 

    x_lag = matrix(nrow = n.obs, ncol = p)
    for (j in 1:p){
      for (i in 1:n.obs){
        x_lag[i,j] = data[p+i-j]
      }
    }
    beta_hat = solve(crossprod(x_lag), crossprod(x_lag, y))
      fc[k+1] = data[(length(data[1:idx]) +k): (length(data[1:idx]) +k - p +1)] %*% beta_hat
  }
  print(list("fc" = fc))
  }

fit = forecast(y, 11, 1, idx)

loss = function(fc, y_test){
  print(mae(y_test, fc))
  print(rmse(y_test, fc))
}

loss(fit$fc, y[-c(1:idx)])

plot(fit$fc, type ="l", ylim = c(-0.5, 0.5), xlim = c(0, 140), col = "red")
par(new = TRUE)
plot(y[-c(1:idx)], type ="l", ylim = c(-0.5, 0.5), xlim = c(0, 140))
