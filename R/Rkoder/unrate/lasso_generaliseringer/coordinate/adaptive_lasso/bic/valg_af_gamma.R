source("lasso_generaliseringer/coordinate/lasso/bic/insample.R")

beta_lasso = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]
idx_lasso = which(beta_lasso != 0)
v_l0 = 1/abs(beta_lasso[idx_lasso])^0.5 
v_l1 = 1/abs(beta_lasso[idx_lasso])
v_l2 = 1/abs(beta_lasso[idx_lasso])^2 

adap_lasso_fit0 = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v_l0)

adap_lasso_bic0 = lassoBIC(y_train, x_train[,idx_lasso], adap_lasso_fit0)

adap_lasso_fit1 = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                         family = "gaussian", alpha = 1, standardize = FALSE, 
                         penalty.factor = v_l1)

adap_lasso_bic1 = lassoBIC(y_train, x_train[,idx_lasso], adap_lasso_fit1)

adap_lasso_fit2 = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                         family = "gaussian", alpha = 1, standardize = FALSE, 
                         penalty.factor = v_l2)

adap_lasso_bic2 = lassoBIC(y_train, x_train[,idx_lasso], adap_lasso_fit2)

bic = c(adap_lasso_bic0$bic_min, adap_lasso_bic1$bic_min, adap_lasso_bic2$bic_min)
which.min(bic)
