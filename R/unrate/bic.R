BIC <- function(y, X, fit, i) {
  n.obs <- length(y)
  sigma2est <- sum((y  - X %*% fit$beta[,i])^2) / n.obs
  BIC <- log(sigma2est) + fit$df[i] * log(n.obs) / n.obs
  return(BIC)
}

BIC(y, x, glmnet(x, y, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE), 1)
