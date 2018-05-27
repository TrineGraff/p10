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

factorForecast <- function(y, X, idx = idx, m = 4, r.hat) {
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


fc.IC.1 =  factorForecast(y, xf, idx = idx, m = 4, r = 6)
c = tidy(fc.IC.1[,1])
write.csv(c$x, file = "fc_faktor_ic1") 


fc.IC.2 = factorForecast(y, xf, idx = idx, m = 4, r = 11)
c = tidy(fc.IC.2[,1])
write.csv(c$x, file = "fc_faktor_ic2") 

fc.IC.3 = factorForecast(y, xf, idx = idx, m = 4, r = 20)
c = tidy(fc.IC.3[,1])
write.csv(c$x, file = "fc_faktor_ic3") 


# Analyse -----------------------------------------------------------------

dato = c(as.character(dato_test))
df = data.frame(date = as.Date(dato), fc.IC.2, y_test)

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y_test, colour = "Arbejdsløshedsrate")) +
  geom_line(aes(y = fc.IC.2, colour = "Benchmark model")) +
  ylab("Rate") + xlab("") +
  scale_colour_manual(values = c("gray", "red")) +
  theme(legend.title=element_blank())


expression(paste("Value is ", sigma,",", R^{2},'=0.6'))

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

round(mae(fc.IC.1 - y_test), digits = 4)
round(mse(fc.IC.1 - y_test), digits = 4)

round(mae(fc.IC.2 - y_test), digits = 4)
round(mse(fc.IC.2 - y_test), digits = 4)

round(mae(fc.IC.3 - y_test), digits = 4)
round(mse(fc.IC.3 - y_test), digits = 4)
