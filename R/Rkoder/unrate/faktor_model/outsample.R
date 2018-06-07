source("package.R")
source("data_unrate.R")

getFactors <- function(X, r) {
  p = ncol(X) 
  XTX = crossprod(X)
  
  X.eig = eigen(XTX, symmetric = TRUE)
  eig.vec = X.eig$vectors
  
  loadings = eig.vec[, 1:r] * sqrt(p)
  factors = (X %*% loadings) / p
  
  list.out <- list(
    "factors" = factors ,
    "loading" = loadings
  )
  return(list.out)
}

ForecastFaktor <- function(y, X, idx = idx, m = 4, r.hat) {
  df.y.lags = foreach(i = 1:m, .combine = cbind) %do%{
    lag(y, i) 
  }
  n.obs = length(y_test) 
  
  fc = foreach(i = 1:n.obs, .combine = rbind) %do%{
    X = xf[1:(idx + i),]
    w.t = df.y.lags[1:(idx + i), ]
    F.t = getFactors(X, r.hat)$factors
    Z = data.frame(F.t, w.t) 
    
    Z.in = Z[(m + 1):(idx + i - 1), ] %>% as.matrix(.)
    Y.res = y[(m + 1):(idx + i - 1)]
    Z.out = Z[(idx + i), ] %>% as.matrix(.)
    
    ZTZ = crossprod(Z.in)
    ZY = crossprod(Z.in, Y.res)
    beta = solve(ZTZ, ZY)
    
    Y.fc <- Z.out %*% beta
    return(Y.fc)
  }
  print(fc)
}
