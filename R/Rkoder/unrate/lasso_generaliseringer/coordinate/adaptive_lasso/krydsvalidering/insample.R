source("lasso_generaliseringer/coordinate/lasso/krydsvalidering/insample.R")

beta_lasso = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_lasso = which(beta_lasso != 0)
v_l = 1/abs(beta_lasso[idx_lasso])^0.5 
 
adap_lasso_fit = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)


adap_lasso_cv = cv.glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                     family = "gaussian", alpha = 1, standardize = FALSE, 
                     penalty.factor = v_l)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(adap_lasso_cv$lambda.min), log(adap_lasso_cv$lambda.1se)),
  error = with(adap_lasso_cv, c(cvm[which(lambda == adap_lasso_cv$lambda.min)], cvm[which(lambda == adap_lasso_cv$lambda.1se)])),  
  p = apply(coef(adap_lasso_fit, s = c(adap_lasso_cv$lambda.min, adap_lasso_cv$lambda.1se)), 2, parm) 
) 



