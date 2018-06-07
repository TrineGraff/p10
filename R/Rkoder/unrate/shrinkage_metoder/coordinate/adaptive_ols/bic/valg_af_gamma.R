source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_0 = 1/abs(coef$`fit_ols$coefficients`)^0.5
v_1 = 1/abs(coef$`fit_ols$coefficients`)^1
v_2 = 1/abs(coef$`fit_ols$coefficients`)^2

adap_ols_fit_0 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_0)
adap_ols_bic_0 = lassoBIC(y_train, x_train, adap_ols_fit_0)

adap_ols_fit_1 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_1)
adap_ols_bic_1 = lassoBIC(y_train, x_train, adap_ols_fit_1)

adap_ols_fit_2 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_2)
adap_ols_bic_2 = lassoBIC(y_train, x_train, adap_ols_fit_2)


bic = c(adap_ols_bic_0$bic_min, adap_ols_bic_1$bic_min, adap_ols_bic_2$bic_min)
which.min(bic)
