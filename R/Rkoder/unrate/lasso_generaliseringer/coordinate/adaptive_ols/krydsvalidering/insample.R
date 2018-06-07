source("package.R")
source("parm.R")
source("data_unrate.R")
set.seed(1)

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_0 = 1/abs(coef$`fit_ols$coefficients`)^0.5

adap_ols_cv = cv.glmnet(x_train, y_train, alpha = 1, penalty.factor = v_0,
                        intercept = FALSE, family = "gaussian", standardize = FALSE )
adap_ols_fit = glmnet(x_train, y_train, alpha = 1, penalty.factor = v_0, 
                      intercept = FALSE, family = "gaussian", standardize=FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(adap_ols_cv$lambda.min), log(adap_ols_cv$lambda.1se)),
  error = with(adap_ols_cv, c(cvm[which(lambda == adap_ols_cv$lambda.min)], cvm[which(lambda == adap_ols_cv$lambda.1se)])),  
  p = apply(coef(adap_ols_fit, s = c(adap_ols_cv$lambda.min, adap_ols_cv$lambda.1se)), 2, parm) 
) 
