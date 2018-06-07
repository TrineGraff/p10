source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

coef_kryds = coef(lasso_fit, s = lasso_cv$lambda.1se)[-1]

n <- length(y_train)
lambda = lasso_cv$lambda.1se * n
fixed_lasso_kryds = fixedLassoInf(x_train, y_train, coef_kryds, lambda = lambda, 
                                  intercept = FALSE, alpha = 0.1)


