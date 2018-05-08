source("data_unrate.R")
library(foreach)

fc = function(data, p, idx){
  n.test = length(y_test)
  n.res = length(data) 
  x.lag = matrix(nrow = n.res, ncol = p)
  for (j in 1 : p){
    for (i in 1 : n.res){
      x.lag[i, j] = data[p + i - j]
    }
  }
  fc = foreach(k = 1 : n.test, .combine = rbind) %do%{
    x.in = x.lag[1:(idx + k - 1), ] 
    y.res = data[1:(idx + k - 1)]
    
    #x.out = x.lag[idx + k, ]
    
    # Regression Model 
    beta.hat = solve(crossprod(x.in), crossprod(x.in, y.res))
    #print(beta.hat)
    #pred_test = x.out %*% beta.hat
    
    #return(pred_test)
  }
  print(beta.hat)
  #print(fc)
}

fc(y, 4, idx = idx)





# Forecast - AR -----------------------------------------------------------
forecast_ar = function(data, p, idx = idx) {
  fc = c(NA)
  for(k in 0:length(data[-c(1:idx)]) - 1) {
    y = data[(p + 1):(length(data[1:idx]) + k)] 
    n = length(y) 
    x_lag = matrix(nrow = n, ncol = p)
    for (j in 1 : p){
      for (i in 1 : n){
        x_lag[i, j] = data[p + i - j]
      }
    }
    beta_hat = solve(crossprod(x_lag), crossprod(x_lag, y))
      fc[k+1] = data[(length(data[1:idx]) + k):(length(data[1:idx]) + k - p + 1)] %*% beta_hat
  }
  print(list("fc" = fc))
  }

fit = forecast_ar(y, 4, idx)






plot(fit$fc, type = "l", col = "red", xlim = c(0,140), ylim = c(-0.5, 0.5))
par(new = TRUE)
plot(y_test, type = "l", xlim = c(0,140), ylim = c(-0.5, 0.5))

# plot --------------------------------------------------------------------

dato = c(as.character(test_dato))

df = data.frame(date = as.Date(dato), fc = fit$fc, y =  y[-c(1:idx)])

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y, colour = "Arbejdsløshed")) +
  geom_line(aes(y = fc, colour = "AR(11)")) +
  ylab("Rate") + xlab("Dato") +
  scale_colour_manual(values = c("red", "gray")) +
  theme(legend.title=element_blank()) +
  ggtitle("Rolling forecast") 

# test --------------------------------------------------------------------

library(e1071)
library(tseries)
skewness(y)
kurtosis(y) # ex kurtosis
jarque.bera.test(y)

library(stats)
Box.test(y, lag = 10, "Ljung-Box")
Box.test(y^2, lag = 10, "Ljung-Box")
