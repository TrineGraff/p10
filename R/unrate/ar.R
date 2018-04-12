source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
y = data$UNRATE[1:idx]
x = lag(y, k = 1)

x.lag = function(x, p.max){
  BIC.vektor = c(NA)
  
for (p in 1:p.max){
  y = x[(p.max + 1):length(x)]
  n.obs = length(y)
  
  x.lag = matrix(nrow = n.obs, ncol = p)
  for (j in 1:p){
    for (i in 1:n.obs){
      x.lag[i,j] = x[p+i-j]
    }
  }
  beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
  print(beta.hat)
  sigma2.hat = mean((y- x.lag %*% beta.hat)^2)
  BIC = log(sigma2.hat) + (p * log(n.obs)/n.obs)
  BIC.vektor[p] = BIC
}
  print(which.min(BIC.vektor))
}

x.lag(y, 12)

beta.hat = function(x, p) {
  y = x[(p + 1):length(x)]
  n.obs = length(y)
  
  x.lag = matrix(nrow = n.obs, ncol = p)
  for (j in 1:p){
    for (i in 1:n.obs){
      x.lag[i,j] = x[p+i-j]
    }
  }
  beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
  sigma2.hat = mean((y- x.lag %*% beta.hat)^2)
  
  print(list("beta.hat" = beta.hat ))
}

beta.hat(y, 11)
