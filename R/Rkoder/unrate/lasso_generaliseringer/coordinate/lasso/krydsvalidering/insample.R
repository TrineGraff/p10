source("package.R")
source("parm.R")
source("data_unrate.R")
set.seed(1)

lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize = FALSE)
lasso_cv = cv.glmnet(x_train, y_train,  family = "gaussian", alpha = 1, 
                     intercept = FALSE, standardize = FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(lasso_cv$lambda.min), log(lasso_cv$lambda.1se)),
  error = with(lasso_cv, c(cvm[which(lambda == lasso_cv$lambda.min)], cvm[which(lambda == lasso_cv$lambda.1se)])),  
  p = apply(coef(lasso_fit, s = c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 2, parm) 
) 

