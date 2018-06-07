source("data_unrate.R")
source("package.R")

opt.orden = function(x, p.max){
  BIC.vektor = rep(NA, p.max)
  
  for (p in 1:p.max){
    y = x[(p + 1):length(x)]
    n.obs = length(y)
    x.lag = matrix(nrow = n.obs, ncol = p)
    for (j in 1:p){
      for (i in 1:n.obs){
        x.lag[i,j] = x[p+i-j]
      }
    }
    beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
    sigma2.hat = mean((y - x.lag %*% beta.hat)^2)
    BIC = log(sigma2.hat) + (p * log(n.obs) / n.obs)
    BIC.vektor[p] = BIC
  }
  print(which.min(BIC.vektor))
}

opt.orden(ya_train, 12)

beta = function(x, p, summary = F, fit = F) {
  y = x[(p + 1):length(x)]
  n.obs = length(y)
  
  x.lag = matrix(nrow = n.obs, ncol = p)
  for (j in 1 : p){
    for (i in 1 : n.obs){
      x.lag[i, j] = x[p + i - j]
    }
  }
  
  beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
  
  if(fit){
  fit = x.lag %*% beta.hat
  resid = y - fit
  return(list("beta.hat" = beta.hat, "fit" = fit, "residuals" = resid))
  }

  if(summary){
    sigma2.hat = mean((y - x.lag %*% beta.hat)^2)  
    std.error = sqrt(diag(sigma2.hat * solve(crossprod(x.lag))))
    t.value = beta.hat / std.error
    p.value = pnorm(- abs(beta.hat) / std.error) * 2
    
    BIC = log(sigma2.hat) + (p * log(n.obs) / n.obs)
    SS.res = sum((y - x.lag %*% beta.hat)^2)
    SS.tot = sum((y - mean(y))^2)
    R.sqrd = 1 - (SS.res / SS.tot)
    adj.R.sqrt = 1 - (1 - R.sqrd) * ((n.obs - 1) / (n.obs - p - 1))
    ml.sigma = sqrt(sigma2.hat) * sqrt((n.obs - p) / n.obs)
    fit = x.lag %*% beta.hat
    loglike = sum(log(dnorm(y, mean = fit, sd = ml.sigma)))
  
    return(cbind.data.frame(round(beta.hat, digits = 4), std.error, t.value, p.value,
                           sigma2.hat, BIC, R.sqrd, adj.R.sqrt, loglike))

  }
}

beta(y_train, 4, summary = T, fit = F)
