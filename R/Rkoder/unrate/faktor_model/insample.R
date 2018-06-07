source("package.R")
source("data_unrate.R")

#Bruges xf data
getFactors <- function(X, k) {
  p = ncol(X) 
  XTX = crossprod(X)
  
  X.eig = eigen(XTX, symmetric = TRUE)
  eig.vec = X.eig$vectors
  
  loadings = eig.vec[, 1:k] * sqrt(p)
  factors = (X %*% loadings) / p
  
  list.out <- list(
    "factors" = factors ,
    "loading" = loadings
  )
  return(list.out)
}

estFactors <- function(X, ic = 1, trace = FALSE) {
  X = as.matrix(X.df)
  n.obs = nrow(X)
  p = ncol(X)
  
  k.max = 20
  ics = rep(NA, k.max)
  
  for (k in 1:k.max) {
    est.k = getFactors(X, k)
    F.k = est.k$factors
    L.k = est.k$loading
    
    if(ic == 1) {
      penalty = k * (p + n.obs) / (p * n.obs) * log((p * n.obs) / p + n.obs)
    } else if (ic == 2) {
      penalty = k * (p + n.obs) / (p * n.obs) * log(min(p, n.obs))
    } else if (ic == 3) {
      penalty = k * log(min(p, n.obs)) / min(p, n.obs)
    } else {
      stop("Invalid information criterion argument")
    }
    
    V.k = matrix.trace(crossprod(X - tcrossprod(F.k, L.k))) / (n.obs * p)
    ics[k] = log(V.k) + penalty
    if (trace) cat("k =", k, "\tV =", V.k, "\tIC =", ics[k], "\n")
  }
  
  k.opt = which.min(ics)
  est.opt = getFactors(X, k.opt)
  factors.opt = est.opt$factors
  colnames(factors.opt) = paste("F", 1:k.opt, sep = "")
  df.out = data.frame(factors.opt) %>% tbl_df
  return(df.out)
}


m = 4 #valgt fra en AR
df.y.lags = foreach(i = 1:m, .combine = cbind) %do%{
  lag(yf_train, i) 
}
colnames(df.y.lags) = c("lag1", "lag2", "lag3", "lag4")

fit = function(F.t, w.t, y_train) {
  m = dim(w.t)[2] # antal lags
  n.obs = dim(w.t)[1] #antal observationer t_0
  k = dim(F.t)[2] #antal faktorer
  Z = data.frame(F.t, w.t) %>% tbl_df  
  Z = Z[(m + 1):n.obs,] %>% as.matrix(.) #ser bort fra de første m rækker
  y.res = y_train[(m + 1):n.obs] #ser bort fra de første m observationer
  
  beta.hat = solve(crossprod(Z), crossprod(Z, y.res))
  fit = Z %*% beta.hat
  resid = y.res - fit
  scale_resid = scale(resid)
  
  return(list("beta.hat" = beta.hat, "fit" = fit, "scale_residuals" = scale_resid))
}

