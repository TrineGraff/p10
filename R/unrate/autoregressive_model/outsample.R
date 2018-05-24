source("data_unrate.R")
source("package.R")

# Forecast - AR -----------------------------------------------------------
ForecastAR= function(data, p, idx = idx) {
  fc = c(NA)
  for(k in 0:length(data[-c(1:idx)]) - 1) {
    y = data[(p + 1):(length(data[1:idx]) + k)] 
    n = length(y) 
    x.lag = matrix(nrow = n, ncol = p)
    for (j in 1:p){
      for (i in 1:n){
        x.lag[i, j] = data[p + i - j]
      }
    }
    beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
    fc[k+1] = data[(length(data[1:idx]) + k):(length(data[1:idx]) + k - p + 1)] %*% beta.hat
  }
  print(list("fc" = fc))
  }
fc_ar = ForecastAR(y, 4, idx = idx )

fc_ar = data.frame(fc_ar)
c = tidy(fc_ar$fc)
write.csv(c, file = "ar_4") 


plot(fc$fc, type = "l", col = "red", xlim = c(0,140), ylim = c(-0.5, 0.5))
par(new = TRUE)
plot(y_test, type = "l", xlim = c(0,140), ylim = c(-0.5, 0.5))

# plot --------------------------------------------------------------------

dato = c(as.character(dato_test))

df = data.frame(date = as.Date(dato), fc = fc_ar$fc, y = y_test)

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y, colour = "Arbejdsløshed")) +
  geom_line(aes(y = fc, colour = "AR(4)")) +
  ylab("Rate") + xlab("Dato") +
  scale_colour_manual(values = c("red", "gray")) +
  theme(legend.title=element_blank())

# Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Root Mean Squared Error
mse <- function(error)
{
  (mean(error^2))
}

mae(fc_ar$fc - y_test)
mse(fc_ar$fc - y_test)

