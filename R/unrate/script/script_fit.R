source("data_unrate.R")
library(gglasso)
library(glmnet)
# ridge -------------------------------------------------------------------
lambda_ridge = read.csv("results/ridge_lambda.csv") %>% .[1, 2]
ridge_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 0, 
                   intercept = FALSE, standardize=FALSE, lambda = lambda_ridge)

# lasso -------------------------------------------------------------------
lambda_lasso = read.csv("results/lasso_lambda.csv") %>% .[1, 2]
lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize = FALSE, lambda = lambda_lasso)

# elastik net -------------------------------------------------------------
lambda_el = read.csv("results/el_lambda.csv") %>% .[1, 2]
el_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 0.9, 
                intercept = FALSE, standardize = FALSE, lambda = lambda_el )


# group lasso -------------------------------------------------------------
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 
lambda_grp = read.csv("results/grp_lambda.csv") %>% .[1, 2]
gglasso_fit = gglasso(x_train, y_train, group = grp, intercept = FALSE, loss = "ls")


# adaptive lasso med OLS vægte --------------------------------------------
fit_ols = lm(y ~ 0 + x)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v)

# adaptive lasso med lasso vægte --------------------------------------------
b_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(b_hat != 0) 

v_l = 1/abs(b_hat[idx_hat, ]) #intercept er inkluderet


adap_lasso_fit = glmnet(x[,idx_hat-1], y, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)


