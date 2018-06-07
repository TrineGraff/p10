source("package.R")
source("parm.R")
source("data_unrate.R")
set.seed(1)

ridge_fit = glmnet(x_train, y_train, family = "gaussian", 
                   alpha = 0, intercept = FALSE, standardize=FALSE)
ridge_cv = cv.glmnet(x_train, y_train,family = "gaussian",
                     alpha = 0, intercept = FALSE, standardize=FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(ridge_cv$lambda.min), log(ridge_cv$lambda.1se)),
  error = with(ridge_cv, c(cvm[which(lambda == ridge_cv$lambda.min)], cvm[which(lambda == ridge_cv$lambda.1se)])),  
  p = apply(coef(ridge_fit, s = c(ridge_cv$lambda.min, ridge_cv$lambda.1se)), 2, parm) 
) 


