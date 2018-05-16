source("data_unrate.R")
source("package.R")
source("parm.R")
set.seed(1)

alpha.grid = round(seq(0, 1, length = 10), digits = 2)
for (i in alpha.grid) {
  assign(paste("fit",i , sep=""), cv.glmnet(x_train, y_train, alpha=i, family="gaussian", standardize=FALSE))
}
cv1_min  = c(lambda = fit0$lambda.min, cvm = fit0$cvm[fit0$lambda == fit0$lambda.min], alpha = 0)
cv2_min  = c(lambda = fit0.11$lambda.min, cvm = fit0.11$cvm[fit0.11$lambda == fit0.11$lambda.min], alpha = 0.11)
cv3_min  = c(lambda = fit0.22$lambda.min, cvm = fit0.22$cvm[fit0.22$lambda == fit0.22$lambda.min], alpha = 0.22)
cv4_min  = c(lambda = fit0.33$lambda.min, cvm = fit0.33$cvm[fit0.33$lambda == fit0.33$lambda.min], alpha = 0.33)
cv5_min  = c(lambda = fit0.44$lambda.min, cvm = fit0.44$cvm[fit0.44$lambda == fit0.44$lambda.min], alpha = 0.44)
cv6_min  = c(lambda = fit0.56$lambda.min, cvm = fit0.56$cvm[fit0.56$lambda == fit0.56$lambda.min], alpha = 0.56)
cv7_min  = c(lambda = fit0.67$lambda.min, cvm = fit0.67$cvm[fit0.67$lambda == fit0.67$lambda.min], alpha = 0.67)
cv8_min  = c(lambda = fit0.78$lambda.min, cvm = fit0.78$cvm[fit0.78$lambda == fit0.78$lambda.min], alpha = 0.78)
cv9_min  = c(lambda = fit0.89$lambda.min, cvm = fit0.89$cvm[fit0.89$lambda == fit0.89$lambda.min], alpha = 0.89)
cv10_min  = c(lambda = fit1$lambda.min, cvm = fit1$cvm[fit1$lambda == fit1$lambda.min], alpha = 1)

cv_min = data.frame(cv1_min, cv2_min, cv3_min, cv4_min, cv5_min, cv6_min, cv7_min, cv8_min, cv9_min, cv10_min)
which.min(cv_min[2,] )

el_fit = glmnet(x_train, y_train, alpha=0.89, family="gaussian", standardize=FALSE, intercept = FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(fit0.89$lambda.min), log(fit0.89$lambda.1se)),
  error = with(fit0.89, c(cvm[which(lambda == fit0.89$lambda.min)], cvm[which(lambda == fit0.89$lambda.1se)])),  
  p = apply(coef(el_fit, s = c(fit0.89$lambda.min, fit0.89$lambda.1se)), 2, parm) 
) 

b_hat = coef(el_fit, s = fit0.89$lambda.1se)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


beta_hat = as.vector(coef(el_fit, s = fit0.89$lambda.1se)) %>% .[-1]


# residualer --------------------------------------------------------------

fit = x_train %*% beta_hat 
res = fit - y_train 
res = scale(res)

tmp = data.frame(Date = as.Date(dato_train), y = res)

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

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")
Box.test(res^2, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

SS.res = sum((res)^2)
SS.tot = sum((y_train - mean(y_train))^2)
n = length(y_train)
p = parm(beta_hat)
R.sqrd = 1 - (SS.res / SS.tot)
adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1))

