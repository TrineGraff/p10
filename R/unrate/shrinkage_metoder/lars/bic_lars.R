larsBIC <- function(y, x, fit) {
  BIC <- function(y, x, fit, i) {
    n.obs <- length(y)
    sigma2est <- sum((y - x %*% fit$beta[i,])^2) / n.obs
    BIC <- log(sigma2est) + fit$df[i] * log(n.obs) / n.obs
    return(BIC)
  }
  step = length(fit$lambda) 
  BICs = rep(NA, (step + 1) )
  for (i in 1:(step + 1)) { 
    BICs[i] <- BIC(y, x, fit, i)
  }
  idx = which.min(BICs)
  beta_hat = fit$beta[idx,]
  f = apply(abs(fit$beta), 1, sum) 
  f = f/max(f)
  f_hat = f[idx]
  
  #Adjusted r^2
  SS.res = sum((y - x %*% beta_hat)^2)
  SS.tot = sum((y - mean(y))^2)
  p = parm(beta_hat)
  R.sqrd = 1 - (SS.res / SS.tot)
  n = length(y)
  adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1)) 
  res = y - x %*% beta_hat
  print(list("scale_res" = scale(res),
             "f_hat" = f_hat,
             "bic_min" = BICs[idx], 
             "p" = p,
             "adj.R.sqrt" = adj.R.sqrt
             
  ))
}

lassoBIC <- function(y, x, fit) {
  BIC <- function(y, x, fit, i) {
    n.obs <- length(y)
    sigma2est <- sum((y - x %*% fit$beta[i,])^2) / n.obs
    BIC <- log(sigma2est) + fit$df[i] * log(n.obs) / n.obs
    return(BIC)
  }
  step = length(fit$lambda) 
  BICs = rep(NA, (step + 1) )
  for (i in 1:(step + 1)) { 
    BICs[i] <- BIC(y, x, fit, i)
  }
  idx = which.min(BICs)
  f = apply(abs(fit$beta), 1, sum) 
  f = f/max(f)
  f_hat = f[idx]
  beta_hat = fit$beta[idx,]
  
  #Adjusted r^2
  SS.res = sum((y - x %*% beta_hat)^2)
  SS.tot = sum((y - mean(y))^2)
  p = parm(beta_hat)
  R.sqrd = 1 - (SS.res / SS.tot)
  n = length(y)
  adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1)) 
  res = y - x %*% beta_hat
  print(list("scale_res" = scale(res),
             "f_hat" = f_hat,
             "bic_min" = BICs[idx], 
             "p" = p,
             "adj.R.sqrt" = adj.R.sqrt
             
  ))
}

