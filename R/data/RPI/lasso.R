source("setup_data.R")

fit = glmnet(X, y, family = "gaussian", alpha = 1)
plot_glmnet(fit)

cv_fit = cv.glmnet(X, y, family = "gaussian") 
plot(cv_fit) 


lambda_min = cv_fit$lambda.min
lambda_1se = cv_fit$lambda.1se

leukemia_lambdas <- unlist(cv_fit[c("lambda.min", "lambda.1se")])

parm = function(x) {
  (sum(x != 0))
}

data.frame(
  lambda = c("min", "1se"), 
  error = with(cv_fit, c(cvm[which(lambda == cv_fit$lambda.min)], cvm[which(lambda == cv_fit$lambda.1se)])),  
  p = apply(coef(fit, s = c(cv_fit$lambda.min, cv_fit$lambda.1se)), 2, parm) 
) 
#ønsker mindst kompleksitet. Så vi vælger lambda.1se

b_hat = coef(fit, s = cv_fit$lambda.1se)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]




# lars --------------------------------------------------------------------

fit = lars(X, y, type = "lasso", trace = TRUE)
cv.lars()
plot(fit)
