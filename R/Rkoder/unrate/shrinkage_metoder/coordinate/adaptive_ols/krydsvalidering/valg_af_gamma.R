source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_1 = 1/abs(coef$`fit_ols$coefficients`)
v_0 = 1/abs(coef$`fit_ols$coefficients`)^0.5
v_2 =  1/abs(coef$`fit_ols$coefficients`)^2

adap_ols_cv_0 = cv.glmnet(x_train, y_train, intercept = FALSE, 
                          family = "gaussian", alpha = 1, standardize = FALSE, 
                          penalty.factor = v_0)
adap_ols_fit_0 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v_0)

adap_ols_cv_1 = cv.glmnet(x_train, y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v_1)
adap_ols_fit_1 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_1)

adap_ols_cv_2 = cv.glmnet(x_train, y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v_2)
adap_ols_fit_2 = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_2)
plot(adap_ols_cv_2)


cv0_min  = c(lambda = adap_ols_cv_0$lambda.min, cvm = adap_ols_cv_0$cvm[adap_ols_cv_0$lambda == adap_ols_cv_0$lambda.min], gamma = 0)
cv1_min  = c(lambda = adap_ols_cv_1$lambda.min, cvm = adap_ols_cv_1$cvm[adap_ols_cv_1$lambda == adap_ols_cv_1$lambda.min], gamma = 1)
cv2_min  = c(lambda = adap_ols_cv_2$lambda.min, cvm = adap_ols_cv_2$cvm[adap_ols_cv_2$lambda == adap_ols_cv_2$lambda.min], gamma = 2)
df = rbind(cv0_min, cv1_min, cv2_min)
which.min(df[,2])
#vælger gamma = 0.5






