source("package.R")
source("data_unrate.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/adj.r.2.R")

set.seed(1)

lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE)
lasso_cv = cv.glmnet(x_train, y_train,  family = "gaussian", alpha = 1, standardize=FALSE, intercept = FALSE,)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(lasso_cv$lambda.min), log(lasso_cv$lambda.1se)),
  error = with(lasso_cv, c(cvm[which(lambda == lasso_cv$lambda.min)], cvm[which(lambda == lasso_cv$lambda.1se)])),  
  p = apply(coef(lasso_fit, s = c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 2, parm) 
) 

test = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_lasso = which(test!=0)
beta_hat = as.vector(coef(lasso_fit, s = lasso_cv$lambda.1se)) %>% .[-1] #fjerner skæringen 
fit = x_train %*% beta_hat

# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(res)
#hist <- histogdens.plot(res)
#resid <- residuals.plot(res)
#acf <- residuals.acf.plot(res)
#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                  layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------
coef_min = coef(lasso_fit, s = lasso_cv$lambda.min)
idx_min = which(coef_min != 0)
lm_min = lm(y_train~0 + x_train[, (idx_min - 1)])
summary(lm_min)
logLik(lm_min)

coef_1sd = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_1sd = which(coef_1sd != 0)
lm_1sd = lm(y_train~0 + x_train[, (idx_1sd - 1)])
summary(lm_1sd)
logLik(lm_1sd)

# Koefficienter -----------------------------------------------------------
coef_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(coef_hat != 0) 
coef_hat[idx_hat,]     


# log like ----------------------------------------------------------------
test = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE, s = lasso_cv$lambda.min)
test$nulldev - deviance(test)
?glmnet
