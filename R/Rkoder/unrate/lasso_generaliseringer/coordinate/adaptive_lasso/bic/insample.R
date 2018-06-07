source("lasso_generaliseringer/coordinate/lasso/bic/insample.R")

beta_lasso = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]
idx_lasso = which(beta_lasso != 0)

v_l = 1/abs(beta_lasso[idx_lasso])^0.5 

adap_lasso_fit = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso_bic = lassoBIC(y_train, x_train[,idx_lasso], adap_lasso_fit)

