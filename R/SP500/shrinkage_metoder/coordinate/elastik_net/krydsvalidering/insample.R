source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
set.seed(1)

#alpha.grid = round(seq(0, 1, length = 10), digits = 2)
alpha.grid = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 1)

for (i in alpha.grid) {
  assign(paste("fit",i , sep=""), cv.glmnet(x_train, y_train, alpha=i, family="gaussian", 
                                            standardize=FALSE, intercept = FALSE))
}

cv1_1se  = c(lambda = fit0$lambda.1se, cvm = fit0$cvm[fit0$lambda == fit0$lambda.1se], alpha = 0)
cv2_1se  = c(lambda = fit0.1$lambda.1se, cvm = fit0.1$cvm[fit0.1$lambda == fit0.1$lambda.1se], alpha = 0.1)
cv3_1se  = c(lambda = fit0.2$lambda.1se, cvm = fit0.2$cvm[fit0.2$lambda == fit0.2$lambda.1se], alpha = 0.2)
cv4_1se  = c(lambda = fit0.3$lambda.1se, cvm = fit0.3$cvm[fit0.3$lambda == fit0.3$lambda.1se], alpha = 0.3)
cv5_1se  = c(lambda = fit0.4$lambda.1se, cvm = fit0.4$cvm[fit0.4$lambda == fit0.4$lambda.1se], alpha = 0.4)
cv6_1se  = c(lambda = fit0.5$lambda.1se, cvm = fit0.5$cvm[fit0.5$lambda == fit0.5$lambda.1se], alpha = 0.5)
cv7_1se  = c(lambda = fit0.6$lambda.1se, cvm = fit0.6$cvm[fit0.6$lambda == fit0.6$lambda.1se], alpha = 0.6)
cv8_1se  = c(lambda = fit0.7$lambda.1se, cvm = fit0.7$cvm[fit0.7$lambda == fit0.7$lambda.1se], alpha = 0.7)
cv9_1se  = c(lambda = fit0.8$lambda.1se, cvm = fit0.8$cvm[fit0.8$lambda == fit0.8$lambda.1se], alpha = 0.8)
cv9_1se  = c(lambda = fit0.9$lambda.1se, cvm = fit0.9$cvm[fit0.9$lambda == fit0.9$lambda.1se], alpha = 0.9)
cv10_1se  = c(lambda = fit1$lambda.1se, cvm = fit1$cvm[fit1$lambda == fit1$lambda.1se], alpha = 1)


cv_1se = data.frame(cv1_1se, cv2_1se, cv3_1se, cv4_1se, cv5_1se, cv6_1se, cv7_1se, cv8_1se, cv9_1se, cv10_1se)


data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(fit0.89$lambda.min), log(fit0.89$lambda.1se)),
  error = with(fit0.89, c(cvm[which(lambda == fit0.89$lambda.min)], cvm[which(lambda == fit0.89$lambda.1se)])),  
  p = apply(coef(el_fit, s = c(fit0.89$lambda.min, fit0.89$lambda.1se)), 2, parm) 
) 

beta_hat = as.vector(coef(el_fit, s = fit0.89$lambda.1se)) %>% .[-1]

# residualer --------------------------------------------------------------
fit = x_train %*% beta_hat 
res = y_train - fit 
res = scale(res)

tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(res)
#hist <- histogdens.plot(res)
#resid <- residuals.plot(res)
#acf <- residuals.acf.plot(res)

#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")
Box.test(res^2, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

SS.res = sum((y_train - x_train %*% beta_hat)^2)
SS.tot = sum((y_train - mean(y_train))^2)
n = length(y_train)
p = parm(beta_hat)
R.sqrd = 1 - (SS.res / SS.tot)
adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1)) 
adj.R.sqrt * 100


# Koefficienter -----------------------------------------------------------
coef_hat = coef(el_fit, s = fit0.89$lambda.1se)
idx_hat = which(coef_hat != 0) 
coef_hat[idx_hat, ]


