source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

idx_beta = which(beta_hat != 0)
v_l0 = 1/abs(beta_hat[idx_beta])^0.5 
v_l1 = 1/abs(beta_hat[idx_beta])
v_l2= 1/abs(beta_hat[idx_beta])^2


adap_lasso_fit_0 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l0)

adap_lasso_cv_0 = cv.glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                          family = "gaussian", alpha = 1, standardize = FALSE, 
                          penalty.factor = v_l0)

adap_lasso_fit_1 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                          family = "gaussian", alpha = 1, standardize=FALSE, 
                          penalty.factor = v_l1)

adap_lasso_cv_1 = cv.glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                            family = "gaussian", alpha = 1, standardize = FALSE, 
                            penalty.factor = v_l1)

adap_lasso_fit_2 = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                          family = "gaussian", alpha = 1, standardize=FALSE, 
                          penalty.factor = v_l2)

adap_lasso_cv_2 = cv.glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                            family = "gaussian", alpha = 1, standardize = FALSE, 
                            penalty.factor = v_l2)


cv0_min  = c(lambda = adap_lasso_cv_0$lambda.min, cvm = adap_lasso_cv_0$cvm[adap_lasso_cv_0$lambda == adap_lasso_cv_0$lambda.min], gamma = 0)
cv1_min  = c(lambda = adap_lasso_cv_1$lambda.min, cvm = adap_lasso_cv_1$cvm[adap_lasso_cv_1$lambda == adap_lasso_cv_1$lambda.min], gamma = 1)
cv2_min  = c(lambda = adap_lasso_cv_2$lambda.min, cvm = adap_lasso_cv_2$cvm[adap_lasso_cv_2$lambda == adap_lasso_cv_2$lambda.min], gamma = 2)
df = rbind(cv0_min, cv1_min, cv2_min)
which.min(df[,2])
