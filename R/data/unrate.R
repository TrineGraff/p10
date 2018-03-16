source("setup_data.R")
library(tidyverse)

drops = c("UNRATE")
x = data_train[ , !(colnames(data_train) %in% drops)] 
y = data$UNRATE[1:idx]

parm = function(x) {
  (sum(x != 0))
}
set.seed(1)

# lasso -------------------------------------------------------------------
lasso_fit = glmnet(x, y, family = "gaussian", alpha = 1, intercept = FALSE)
lasso_cv = cv.glmnet(x, y, intercept = FALSE, family = "gaussian", alpha = 1)

idmin = match(lasso_cv$lambda.min, lasso_cv$lambda)
lambda_1se = max(lasso_cv$lambda[idmin], na.rm = TRUE)

plot(lasso_cv)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(lasso_cv$lambda.min, lasso_cv$lambda.1se),
  error = with(lasso_cv, c(cvm[which(lambda == lasso_cv$lambda.min)], cvm[which(lambda == lasso_cv$lambda.1se)])),  
  p = apply(coef(lasso_fit, s = c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 2, parm) 
) 

b_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


# ridge -------------------------------------------------------------------

ridge_fit = glmnet(x, y, family = "gaussian", alpha = 0, intercept = FALSE)
ridge_cv = cv.glmnet(x, y, intercept = FALSE, family = "gaussian", alpha = 0)

plot(ridge_cv)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(ridge_cv$lambda.min, ridge_cv$lambda.1se),
  error = with(ridge_cv, c(cvm[which(lambda == ridge_cv$lambda.min)], cvm[which(lambda == ridge_cv$lambda.1se)])),  
  p = apply(coef(ridge_fit, s = c(ridge_cv$lambda.min, ridge_cv$lambda.1se)), 2, parm) 
) 

# Elastic Net -------------------------------------------------------------------

alpha.grid = seq(0.1, 0.9, length = 10) 

#search <- foreach(i = alpha.grid, .combine = rbind) %dopar% {
 # cv <- cv.glmnet(x, y, family = "gaussian", nfold = 10, alpha = i, intercept = FALSE )
  #vi udfører for hvert alpha cv.glmnet. Ved search[,1] får vi resultatet for glmnet ved et specifikt alpha og lambda (udkommenter det sidste)
  #data.frame(lambda.1se = cv$lambda.1se, lambda.min = cv$lambda.min)
#  data.frame(lambda.min = cv$lambda.min, cvm_min = cv$cvm[cv$lambda == cv$lambda.min], lambda.1se = cv$lambda.1se, cvm_1se = cv$cvm[cv$lambda == cv$lambda.1se], alpha = i)
#}
search

cv_min = search[search$cvm_min == min(search$cvm_min), ]
cv_sd = search[search$cvm_1se == min(search$cvm_1se), ]
fit_min = glmnet(x, y, family = "gaussian", lambda = cv_min$lambda.min, alpha = cv_min$alpha, intercept = FALSE )
fit_sd = glmnet(x, y, family = "gaussian", lambda = cv_sd$lambda.1se, alpha = cv_sd$alpha, intercept = FALSE )


# Group Lasso -------------------------------------------------------------------


