source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

idx_beta = which(beta_hat != 0)
v_l = 1/abs(beta_hat[idx_beta])^0.5 
 
adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)


adap_lasso_cv = cv.glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                     family = "gaussian", alpha = 1, standardize = FALSE, 
                     penalty.factor = v_l)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(adap_lasso_cv$lambda.min), log(adap_lasso_cv$lambda.1se)),
  error = with(adap_lasso_cv, c(cvm[which(lambda == adap_lasso_cv$lambda.min)], cvm[which(lambda == adap_lasso_cv$lambda.1se)])),  
  p = apply(coef(adap_lasso_fit, s = c(adap_lasso_cv$lambda.min, adap_lasso_cv$lambda.1se)), 2, parm) 
) 



