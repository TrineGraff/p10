lassoBIC <- function(y, x, fit) {
  BIC <- function(y, x, fit, i) {
    n.obs <- length(y)
    sigma2est <- sum((y - x %*% fit$beta[,i])^2) / n.obs
    BIC <- log(sigma2est) + fit$df[i] * log(n.obs) / n.obs
    return(BIC)
  }
  n.lambda = length(fit$lambda)
  BICs = rep(NA, n.lambda)
  for (i in 1:n.lambda) {
    BICs[i] <- BIC(y, x, fit, i)
  }
  idx = which.min(BICs)
  beta_hat = fit$beta[,idx]
  res = y - x %*% beta_hat
  print(list("scale_res" = scale(res),
             "log_lambda" = log(fit$lambda[idx]), 
             "lambda" = fit$lambda[idx],
             "bic_min" = BICs[idx], 
             "p" = parm(coef(fit, s = fit$lambda[idx] ))  
             ))
}

