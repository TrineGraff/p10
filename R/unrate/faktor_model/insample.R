source("data_unrate.R")
source("package.R")
source("shrinkage_metoder/res_plot.R")

#xf er de forklarende matricer uden anvendelse af skalering

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

estFactors <- function(X.df, ic = 1, trace = FALSE) {
  X = as.matrix(X.df)
  n.obs = nrow(X)
  p = ncol(X)
  
  k.max = 20
  ics = rep(NA, k.max)
  
  for (k in 1:k.max) {
    est.k <- getFactors(X, k)
    F.k <- est.k$factors
    L.k <- est.k$loading
    
    if(ic == 1) {
      penalty <- k * (p + n.obs) / (p * n.obs) * log((p * n.obs) / p + n.obs)
    } else if (ic == 2) {
      penalty <- k * (p + n.obs) / (p * n.obs) * log(min(p, n.obs))
    } else if (ic == 3) {
      penalty <- k * log(min(p, n.obs)) / min(p, n.obs)
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


# Fittede residualer ------------------------------------------------------
m = 4 #valgt fra en AR

#Laver omega_t
df.y.lags = foreach(i = 1:m, .combine = cbind) %do%{
  lag(yf_train, i) 
}
colnames(df.y.lags) = c("lag1", "lag2", "lag3", "lag4")

#Faktorerne for de forskelle IC
factors.IC.1 = estFactors(xf_train, ic = 1, trace = T) #6 faktorer
factors.IC.2 = estFactors(xf_train, ic = 2, trace = T) #11 faktorer
factors.IC.3 = estFactors(xf_train, ic = 3, trace = T) #20 faktorer

#Finder de fittede værdier og residulerne
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
  
fit.IC.1 = fit(factors.IC.1, df.y.lags, yf_train)
fit.IC.2 = fit(factors.IC.2, df.y.lags, yf_train)
fit.IC.3 = fit(factors.IC.3, df.y.lags, yf_train)

#tester beta og adjusted R squared
df = data.frame(factors.IC.3, df.y.lags)
x.ny = df[(1+m):552,] %>% as.matrix()
y.ny = y_train[(1+m):552]

lm_fit = lm(y.ny~ 0 +x.ny)
logLik(lm_fit)
summary(lm(y.ny~ 0 +x.ny))


# residualer --------------------------------------------------------------
res.IC.1 = scale(fit.IC.1$residuals)
res.IC.2 = scale(fit.IC.2$residuals)
res.IC.3 = scale(fit.IC.3$residuals)

# skal gøres manuelt
tmp = data.frame(Date = as.Date(dato_train), y = res.IC.3)
res = res.IC.3 
  
qqnorm<- qqnorm.plot(res)
hist <- histogdens.plot(res)
resid <- residuals.plot(res)
acf <- residuals.acf.plot(res)

print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
             layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res.IC.1)
kurtosis(res.IC.1)
jarque.bera.test(res.IC.1)
Box.test(res.IC.1, lag = 10, "Ljung-Box")
Box.test(res.IC.1^2, lag = 10, "Ljung-Box")

skewness(res.IC.2)
kurtosis(res.IC.2)
jarque.bera.test(res.IC.2)
Box.test(res.IC.2, lag = 10, "Ljung-Box")
Box.test(res.IC.2^2, lag = 10, "Ljung-Box")

skewness(res.IC.3)
kurtosis(res.IC.3)
jarque.bera.test(res.IC.3)
Box.test(res.IC.3, lag = 10, "Ljung-Box")
Box.test(res.IC.3^2, lag = 10, "Ljung-Box")



# Loadings ----------------------------------------------------------------

loadings = getFactors(xf_train, 6)$loading %>% as.data.frame()
loadings = cbind(colnames(x_train), loadings)

pairs(loadings[,2:7]) 

load_test = loadings[, 2:7] 
load_test[abs(load_test) < 0.5] <- NA

load_test = cbind(colnames(x_train), load_test)


grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 

plot(load_test$V1, load_test$V2, type = "n") # set up plot 
text(load_test[,2:3], labels = load_test$`colnames(x_train)`, cex=.7, col = grp) # add variable names

plot(load_test$V2, load_test$V3, type = "n") # set up plot 
text(load_test[,3:4], labels = load_test$`colnames(x_train)`, cex=.7, col = grp) # add variable names

plot(load_test$V5, load_test$V4, type = "n") # set up plot 
text(load_test[,2:3], labels = load_test$`colnames(x_train)`, cex=.7, col = grp) # add variable names

plot(load_test[,2:5], type="n") # set up plot 
text(load,labels=load_test$`colnames(x_train)`,cex=.7, col = grp) # add variable names
plot(load_test[,2:6], type="n") # set up plot 
text(load,labels=load_test$`colnames(x_train)`,cex=.7, col = grp) # add variable names
plot(load_test[,2:7], type="n") # set up plot 
text(load,labels=load_test$`colnames(x_train)`,cex=.7, col = grp) # add variable names
plot(load_test[,2:8], type="n") # set up plot 
text(load,labels=load_test$`colnames(x_train)`,cex=.7, col = grp) # add variable names


df = data.frame(colnames(x_train), grp, load_test)

df %>% group_by(grp)
df %>% filter(grp == 1)
df %>% filter(grp == 2)
df %>% filter(grp == 3)
df %>% filter(grp == 4)
df %>% filter(grp == 5)
df %>% filter(grp == 6)
df %>% filter(grp == 7)
df %>% filter(grp == 8)


