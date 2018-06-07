source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

beta_test = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]
idx_beta = which(beta_hat != 0)
v_l0 = 1/abs(beta_hat[idx_beta])^0.5 
v_l1 = 1/abs(beta_hat[idx_beta])
v_l2 = 1/abs(beta_hat[idx_beta])^2 

adap_lasso_fit0 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l0)

adap_lasso_bic0 = lassoBIC(y_train, x_train[,idx_beta], adap_lasso_fit0)

adap_lasso_fit1 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                         family = "gaussian", alpha = 1, standardize=FALSE, 
                         penalty.factor = v_l1)

adap_lasso_bic1 = lassoBIC(y_train, x_train[,idx_beta], adap_lasso_fit1)

adap_lasso_fit2 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                         family = "gaussian", alpha = 1, standardize=FALSE, 
                         penalty.factor = v_l2)

adap_lasso_bic2 = lassoBIC(y_train, x_train[,idx_beta], adap_lasso_fit2)

bic = c(adap_lasso_bic0$bic_min, adap_lasso_bic1$bic_min, adap_lasso_bic2$bic_min)
which.min(bic)
