source("package.R")
source("data_unrate.R")
source("parm.R")
source("lasso_generaliseringer/coordinate/bic.R")

fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize = FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)


