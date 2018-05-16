source("data_unrate.R")
source("package.R")

set.seed(1)

# lasso -------------------------------------------------------------------
lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE)
lasso_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, standardize=FALSE)


# ridge -------------------------------------------------------------------
ridge_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 0, intercept = FALSE, standardize=FALSE)
ridge_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 0, standardize=FALSE)

# elasticnet --------------------------------------------------------------

alpha.grid = seq(0.1, 0.9, length = 10)
for (i in alpha.grid) {
  assign(paste("fit",i , sep=""), cv.glmnet(x_train, y_train, alpha=i,family="gaussian", standardize=FALSE))
}

# group lasso -------------------------------------------------------------
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 

gglasso_cv <- cv.gglasso(x_train, y_train, group = grp, nfold = 10, intercept = FALSE, loss = "ls" )
gglasso_fit = gglasso(x_train, y_train, group = grp, intercept = FALSE, loss = "ls")


# adaptive lasso med ols vægte --------------------------------------------
fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols = cv.glmnet(x_train, y_train, intercept = FALSE, 
                     family = "gaussian", alpha = 1, standardize = FALSE, 
                     penalty.factor = v)


adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, standardize=FALSE, penalty.factor = v)




# adaptive lasso med lasso vægte ------------------------------------------
b_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(b_hat != 0) 

v_l = 1/abs(b_hat[idx_hat, ]) #intercept er inkluderet


adap_lasso_fit = glmnet(x[,idx_hat - 1], y, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)



adap_lasso_fit


#library(broom)
#c <- cbind(lambda_min = ridge_cv$lambda.min) %>% tidy()
#write.csv(c, file = "ridge_lambda.csv") 

