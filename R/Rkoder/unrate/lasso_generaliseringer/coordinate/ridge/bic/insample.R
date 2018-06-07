source("package.R")
source("parm.R")
source("data_unrate.R")
source("lasso_generaliseringer/coordinate/bic.R")

fit_ridge = glmnet(x_train, y_train, family = "gaussian", 
                   alpha = 0, intercept = FALSE, standardize=FALSE)
fit_bic_ridge = lassoBIC(y_train, x_train, fit_ridge)


