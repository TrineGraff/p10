<<<<<<< Updated upstream
source("../data/setup_data.R")
=======
source("/Desktop/Projekt/R/data/setup_data.R")
>>>>>>> Stashed changes
library(Metrics)
library(ggplot2)
library(gridExtra)
library(grid)

y = scale(data[, "UNRATE"], scale = FALSE)

# Forecast - AR -----------------------------------------------------------
forecast = function(data, p, idx = idx ) {
  fc = c(NA)
  for(k in 0:length(data[-c(1:idx)])) {
    y = data[(p + 1):(length(data[1:idx]) + k)] #y bliver opdateret med den sande værdi for hvert k
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

fit = forecast(y, 1, idx)

# Analyse -----------------------------------------------------------------
loss = function(fc, y_test){
  print(list("MAE" = mae(y_test, fc)))
  print(list("RMSE" = rmse(y_test, fc)))
}

loss(fit$fc, y = c(y[idx], y[-c(1:idx)]))

# plot --------------------------------------------------------------------

as.character(test_dato)
dato = c(as.character(data_raw$dato[idx][1]), as.character(test_dato))

df = data.frame(date = as.Date(dato), fc = fit$fc, y = c(y[idx], y[-c(1:idx)]))

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y, colour = "Arbejdsløshed")) +
  geom_line(aes(y = fc, colour = "AR(1)")) +
  ylab("Rate") + xlab("Dato") +
  scale_colour_manual(values = c("red", "gray")) +
  theme(legend.title=element_blank()) +
  ggtitle("One-step-ahead forecast") 

# Plot residualer ---------------------------------------------------------

res = scale(fit$fc - c(y[idx], y[-c(1:idx)]))
tmp = data.frame(Date = as.Date(dato), y = res)

qqnorm.plot = function(y){
  q.sample_l = quantile(y)[["25%"]]
  q.sample_u = quantile(y)[["75%"]]
  q.theory_l = qnorm(0.25)
  q.theory_u = qnorm(0.75)
  slope = (q.sample_l - q.sample_u)/(q.theory_l-q.theory_u)
  int = q.sample_l - slope*q.theory_l
  ggplot() +
    stat_qq(aes(sample = y)) +
    geom_abline(intercept = int, slope = slope) +
    xlab("Teoretisk kvantil") +
    ylab("Sample kvantil") +
    ggtitle("Normal Q-Q plot") 
}

histogdens.plot = function(y){
  ggplot(mapping = aes(x = y)) +
    geom_histogram(aes(y = ..density..), binwidth = 1) +
    geom_density() +
    stat_function(fun = dnorm, color="red", args=list(mean=mean(y), sd=sd(y))) +
    xlab("Standard afvigelse") +
    ylab("") +
    ggtitle("Fordelingen af residualerne")
}

residuals.plot = function(y) {
  ggplot(tmp, aes(Date, y)) + 
    geom_line() +
    xlab("Dato") +
    ylab("Standardiserede residualer") +
    ggtitle("Residualerne")
}

residuals.acf.plot = function(y){
  acf <- acf(y, plot = FALSE)
  df <- with(acf, data.frame(lag, acf))
  acf.confint <- qnorm(0.975) / sqrt(length(y))
  acf.plot <- df %>%
    ggplot(aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = c(-acf.confint, acf.confint), linetype = "dashed", color = "blue") +
    xlab("Lags") +
    ylab("Autokorrelation") +
    ggtitle("Residual korrelogram")
}

qqnorm<- qqnorm.plot(res)
hist <- histogdens.plot(res)
resid <- residuals.plot(res)
acf <- residuals.acf.plot(res)

grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
             layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2))



# test --------------------------------------------------------------------

library(e1071)
library(tseries)
skewness(y)
kurtosis(y) # ex kurtosis
jarque.bera.test(y)

library(stats)
Box.test(y, lag = 10, "Ljung-Box")
Box.test(y^2, lag = 10, "Ljung-Box")
