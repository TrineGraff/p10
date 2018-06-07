source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

beta_test = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]
idx_beta = which(beta_hat != 0)
v_l = 1/abs(beta_hat[idx_beta])^0.5 

adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso_bic = lassoBIC(y_train, x_train[,idx_beta], adap_lasso_fit)

