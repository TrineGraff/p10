source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_cv = cv.glmnet(x_train, y_train, intercept = FALSE, 
                     family = "gaussian", alpha = 1, standardize = FALSE, 
                     penalty.factor = v)

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(adap_ols_cv$lambda.min), log(adap_ols_cv$lambda.1se)),
  error = with(adap_ols_cv, c(cvm[which(lambda == adap_ols_cv$lambda.min)], cvm[which(lambda == adap_ols_cv$lambda.1se)])),  
  p = apply(coef(adap_ols_fit, s = c(adap_ols_cv$lambda.min, adap_ols_cv$lambda.1se)), 2, parm) 
) 

beta_hat = as.vector(coef(adap_ols_fit, s = adap_ols_cv$lambda.min)) %>% .[-1]


# residualer --------------------------------------------------------------

fit = x_train %*% beta_hat
res = y_train - fit 
res = scale(res)

#tmp = data.frame(Date = as.Date(dato_train), y = res)
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

coef_min = coef(adap_ols_fit, s = adap_ols_cv$lambda.min)
idx_min = which(coef_min != 0)
lm_min = lm(y_train~0+x_train[, (idx_min - 1)])
summary(lm_min)
logLik(lm_min)

coef_1sd = coef(adap_ols_fit, s = adap_ols_cv$lambda.1se)
idx_1sd = which(coef_1sd != 0)
lm_1sd = lm(y_train~0+x_train[, (idx_1sd - 1)])
summary(lm_1sd)
logLik(lm_1sd)
# Koefficienter -----------------------------------------------------------

coef_hat = coef(adap_ols_fit, s = adap_ols_cv$lambda.min)
idx_hat = which(coef_hat != 0) 
coef_hat[idx_hat, ]


