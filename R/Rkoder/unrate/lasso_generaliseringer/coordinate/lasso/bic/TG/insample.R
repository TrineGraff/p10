source("package.R")
source("data_unrate.R")
source("parm.R")
source("lasso_generaliseringer/coordinate/lasso/bic/insample.R")

n = length(y_train)
lambda = fit_bic_lasso$lambda * n
beta = coef(fit_lasso, s = lambda/n)[-1]
fixed_glm = fixedLassoInf(x_train, y_train, beta, lambda, 
                           intercept = FALSE, alpha = 0.1)
