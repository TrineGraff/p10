source("data_unrate.R")
library(ggplot2)
library(gridExtra)
library(grid)

## Vi anvender ols til at fitte med
## Bestemmer orden i x.lag. Lader p_max = 12, og anvender træningsdata
x.lag = function(x, p.max){
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

x.lag(y_train, 12)

# Udfra overstående funktion er p bestemt.

## Vi finder beta baseret på træningsmængden og summary(regression)
beta = function(x, p, summary = F, fit = F) {
  y = x[(p + 1):length(x)]
  n.obs = length(y)
  
  #designmatricen
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
  
    return(cbind.data.frame(beta.hat, std.error, t.value, p.value,
                           sigma2.hat, BIC, R.sqrd, adj.R.sqrt, loglike
                           ))

  }
  

}

beta(y_train, 4, summary = T, fit = F)

## Vi tjekker om vi får samme resultater, som de indbyggede funktioner
#Får samme estimater med ar-funktionen
fit_ar = ar(y_train, method = "ols", order.max = 4, demean = FALSE)

#Får samme estimater, samt summary resultater. Der er en difference i det 4 decimal. 
y_lm = y_train[(4 + 1):length(y_train)]
n = length(y_lm)
x_lm = matrix(nrow = n, ncol = 4)
for (j in 1:4){
  for (i in 1:n){
    x_lm[i,j] = y_train[4+i-j]
  }
}

beta_hat = solve(crossprod(x_lm), crossprod(x_lm, y_lm))
lm = lm(y_lm ~ 0 + x_lm)
lm$residuals
summary(lm)
logLik(lm)


## Ser på residualerne
# ens med lm$resid
fit = beta(y_train, 4, summary = F, fit = T) 
res = fit$residuals

tmp = data.frame(Date = as.Date(train_dato[5:length(y_train)]), y = fit$residuals)

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

grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
             layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2))




