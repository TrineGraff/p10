source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_2 = 1/abs(coef$`fit_ols$coefficients`)^2

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_2)
adap_ols_bic = lassoBIC(y_train, x_train, adap_ols_fit)

