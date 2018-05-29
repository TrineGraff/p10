source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

idx_beta = which(beta_hat != 0)
v_l = 1/1/abs(beta_hat[idx_beta]) 
 
adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)


adap_lasso_cv = cv.glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                     family = "gaussian", alpha = 1, standardize = FALSE, 
                     penalty.factor = v_l)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(adap_lasso_cv$lambda.min), log(adap_lasso_cv$lambda.1se)),
  error = with(adap_lasso_cv, c(cvm[which(lambda == adap_lasso_cv$lambda.min)], cvm[which(lambda == adap_lasso_cv$lambda.1se)])),  
  p = apply(coef(adap_lasso_fit, s = c(adap_lasso_cv$lambda.min, adap_lasso_cv$lambda.1se)), 2, parm) 
) 


beta_hat = as.vector(coef(adap_lasso_fit, s = adap_lasso_cv$lambda.min)) %>% .[-1]

# Residualer  -------------------------------------------------------------

fit = x_train[,idx_beta] %*% beta_hat
res = y_train - fit 
res = scale(res)

tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(res)
#hist <- histogdens.plot(res)
#resid <- residuals.plot(res)
#acf <- residuals.acf.plot(res)

#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
 #                  layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))


skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------
adj.r.2_min = adj.r.2(y_train, x_train[,idx_beta], beta_hat)

coef_1sd = as.vector(coef(adap_lasso_fit, s = adap_lasso_cv$lambda.1se)) %>% .[-1]
adj.r.2(y_train, x_train[,idx_beta], coef_1sd)


# koefficienter -----------------------------------------------------------
b_hat = coef(adap_lasso_fit, s = adap_lasso_cv$lambda.min)
idx_hat_l = which(b_hat != 0) 
b_hat[idx_hat_l, ]

