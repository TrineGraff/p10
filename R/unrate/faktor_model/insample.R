source("data_unrate.R")
source("package.R")

#xf er de forklarende matricer uden anvendelse af skalering

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

estFactors <- function(X.df, ic = 1, trace = FALSE) {
  X = as.matrix(X.df)
  n.obs = nrow(X)
  p = ncol(X)
  
  r.max = 20
  ics = rep(NA, r.max)
  
  for (r in 1:r.max) {
    est.r <- getFactors(X, r)
    F.r <- est.r$factors
    L.r <- est.r$loading
    
    if(ic == 1) {
      penalty <- r * (p + n.obs) / (p * n.obs) * log((p * n.obs) / p + n.obs)
    } else if (ic == 2) {
      penalty <- r * (p + n.obs) / (p * n.obs) * log(min(p, n.obs))
    } else if (ic == 3) {
      penalty <- r * log(min(p, n.obs)) / min(p, n.obs)
    } else {
      stop("Invalid information criterion argument")
    }
    
    V.r = matrix.trace(crossprod(X - tcrossprod(F.r, L.r))) / (n.obs * p)
    ics[r] = log(V.r) + penalty
    if (trace) cat("r =", r, "\tV =", V.r, "\tIC =", ics[r], "\n")
  }
  
  r.opt = which.min(ics)
  est.opt = getFactors(X, r.opt)
  factors.opt = est.opt$factors
  colnames(factors.opt) = paste("F", 1:r.opt, sep = "")
  df.out = data.frame(factors.opt) %>% tbl_df
  return(df.out)
}

m = 4 #valgt fra en AR
#Laver omega_t
df.y.lags = foreach(i = 1:m, .combine = cbind) %do%{
  lag(y_train, i) 
}
colnames(df.y.lags) = c("lag1", "lag2", "lag3", "lag4")

#Faktorerne for de forskelle IC
factors.IC.1 = estFactors(xf_train, ic = 1, trace = F) #6 faktorer
factors.IC.2 = estFactors(xf_train, ic = 2, trace = F) #11 faktorer
factors.IC.3 = estFactors(xf_train, ic = 3, trace = F) #20 faktorer

#Finder de fittede værdier og residulerne
fit = function(F.t, w.t, y_train) {
  m = dim(w.t)[2] # antal lags
  n.obs = dim(w.t)[1] #antal observationer t_0
  r = dim(F.t)[2] #antal faktorer
  Z = data.frame(F.t, w.t) %>% tbl_df  
  Z = Z[(m + 1):n.obs,] %>% as.matrix(.) #ser bort fra de første m rækker
  y.res = y_train[(m + 1):n.obs] #ser bort fra de første m observationer
  
  beta.hat = solve(crossprod(Z), crossprod(Z, y.res))
  fit = Z %*% beta.hat
  resid = y.res - fit
  
  SS.res = sum((resid)^2)
  SS.tot = sum((y.res - mean(y.res))^2)
  R.sqrd = 1 - (SS.res / SS.tot)
  adj.R.sqrt = 1 - (1 - R.sqrd) * ((n.obs - 1) / (n.obs - (m + r) - 1))
  
  
  return(list("beta.hat" = beta.hat, "fit" = fit, "residuals" = resid, 
              "adj.R.sqrt" = adj.R.sqrt))
  }
  
fit.IC.1 = fit(factors.IC.1, df.y.lags, y_train)
fit.IC.2 = fit(factors.IC.2, df.y.lags, y_train)
fit.IC.3 = fit(factors.IC.3, df.y.lags, y_train)

#tester beta og adjusted R squared
df = data.frame(factors.IC.1, df.y.lags)
x.ny = df[(1+m):552,] %>% as.matrix()
y.ny = y_train[(1+m):552]

summary(lm(y.ny~ 0 +x.ny))


# residualer --------------------------------------------------------------
res.IC.1 = scale(fit.IC.1$residuals)
res.IC.2 = scale(fit.IC.2$residuals)
res.IC.3 = scale(fit.IC.3$residuals)

# skal gøres manuelt
tmp = data.frame(Date = as.Date(train_dato[5:length(y_train)]), y = res.IC.3)
res = res.IC.3 
  
qqnorm.plot = function(y){
  q.sample_l = quantile(y)[["25%"]]
  q.sample_u = quantile(y)[["75%"]]
  q.theory_l = qnorm(0.25)
  q.theory_u = qnorm(0.75)
  slope = (q.sample_l - q.sample_u)/(q.theory_l-q.theory_u)
  int = q.sample_l - slope*q.theory_l
  ggplot() +
    stat_qq(aes(sample = y)) +
    geom_abline(intercept = int, slope = slope) +
    xlab("Teoretisk kvantil") +
    ylab("Sample kvantil") +
    ggtitle("Normal Q-Q plot") 
}

histogdens.plot = function(y){
  ggplot(mapping = aes(x = y)) +
    geom_histogram(aes(y = ..density..), binwidth = 1) +
    geom_density() +
    stat_function(fun = dnorm, color="red", args=list(mean=mean(y), sd=sd(y))) +
    xlab("Standard afvigelse") +
    ylab("") +
    ggtitle("Fordelingen af residualerne")
}

residuals.plot = function(y) {
  ggplot(tmp, aes(Date, y)) + 
    geom_line() +
    xlab("Dato") +
    ylab("Residualer") +
    ggtitle("Residualerne")
}

residuals.acf.plot = function(y){
  acf <- acf(y, plot = FALSE)
  df <- with(acf, data.frame(lag, acf))
  acf.confint <- qnorm(0.975) / sqrt(length(y))
  acf.plot <- df %>%
    ggplot(aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = c(-acf.confint, acf.confint), linetype = "dashed", color = "blue") +
    xlab("Lags") +
    ylab("Autokorrelation") +
    ggtitle("Residual korrelogram")
}

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


## Ser på loadings
loadings = getFactors(xf_train, 6)$loading %>% as.data.frame()
loadings = cbind(colnames(x_train), loadings)


plot(loadings$V1, loadings$V3)

load <- loadings[,4:3] 
plot(load, type="n") # set up plot 
text(load,labels=loadings$`colnames(x_train)`,cex=.7) # add variable names


factor.plot(loadings[,2:7], cluster = NULL, cut = 0, labels=NULL) 

?pairs
pairs(loadings[,2:7])
text(load,labels=loadings$`colnames(x_train)`,cex=.7) # add variable names
