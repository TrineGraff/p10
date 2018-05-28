source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
set.seed(1)

alpha.grid = round(seq(0, 1, length = 10), digits = 2)
alpha.grid = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 1)


for (i in alpha.grid) {
  assign(paste("fit",i , sep=""), cv.glmnet(x_train, y_train, alpha=i, family="gaussian", 
                                            standardize=FALSE, intercept = FALSE))
}
cv_min  = c(lambda = fit_test$lambda.min, cvm = fit_test$cvm[fit_test$lambda == fit_test$lambda.min], alpha = 0.9)
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

cv_min  = c(lambda = fit_test$lambda.1se, cvm = fit_test$cvm[fit_test$lambda == fit_test$lambda.1se], alpha = 0.9)
cv1_min  = c(lambda = fit0$lambda.1se, cvm = fit0$cvm[fit0$lambda == fit0$lambda.1se], alpha = 0)
cv2_min  = c(lambda = fit0.1$lambda.1se, cvm = fit0.11$cvm[fit0.1$lambda == fit0.1$lambda.1se], alpha = 0.1)
cv3_min  = c(lambda = fit0.2$lambda.1se, cvm = fit0.2$cvm[fit0.2$lambda == fit0.2$lambda.1se], alpha = 0.2)
cv4_min  = c(lambda = fit0.3$lambda.1se, cvm = fit0.3$cvm[fit0.3$lambda == fit0.3$lambda.1se], alpha = 0.3)
cv5_min  = c(lambda = fit0.4$lambda.1se, cvm = fit0.4$cvm[fit0.4$lambda == fit0.4$lambda.1se], alpha = 0.4)
cv6_min  = c(lambda = fit0.5$lambda.1se, cvm = fit0.5$cvm[fit0.5$lambda == fit0.5$lambda.1se], alpha = 0.5)
cv7_min  = c(lambda = fit0.6$lambda.1se, cvm = fit0.6$cvm[fit0.6$lambda == fit0.67$lambda.1se], alpha = 0.67)
cv8_min  = c(lambda = fit0.78$lambda.1se, cvm = fit0.78$cvm[fit0.78$lambda == fit0.78$lambda.1se], alpha = 0.78)
cv9_min  = c(lambda = fit0.89$lambda.1se, cvm = fit0.89$cvm[fit0.89$lambda == fit0.89$lambda.1se], alpha = 0.89)
cv10_min  = c(lambda = fit1$lambda.1se, cvm = fit1$cvm[fit1$lambda == fit1$lambda.1se], alpha = 1)







cv_min = data.frame(cv1_min, cv2_min, cv3_min, cv4_min, cv5_min, cv6_min, cv7_min, cv8_min, cv9_min, cv10_min)

el_fit = glmnet(x_train, y_train, alpha=0.89, family="gaussian", 
                standardize=FALSE, intercept = FALSE)

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


