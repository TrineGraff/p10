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

