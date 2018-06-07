source("package.R")
source("parm.R")
source("data_unrate.R")
source("lasso_generaliseringer/coordinate/bic.R")

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_2 = 1/abs(coef$`fit_ols$coefficients`)^2

adap_ols_fit = glmnet(x_train, y_train, alpha = 1, penalty.factor = v_2,
                      intercept = FALSE, family = "gaussian",  standardize = FALSE)
adap_ols_bic = lassoBIC(y_train, x_train, adap_ols_fit)

