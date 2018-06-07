source("lasso_generaliseringer/coordinate/lasso/krydsvalidering/insample.R")

n <- length(y_train)
coef = coef(lasso_fit, s = lasso_cv$lambda.1se)[-1]
lambda = lasso_cv$lambda.1se * n
fixed_lasso_kryds = fixedLassoInf(x_train, y_train, coef, lambda = lambda, 
                                  intercept = FALSE, alpha = 0.1)


