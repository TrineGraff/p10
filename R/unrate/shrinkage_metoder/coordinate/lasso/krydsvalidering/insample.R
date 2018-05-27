source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")

set.seed(1)

lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE)
lasso_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, standardize=FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(lasso_cv$lambda.min), log(lasso_cv$lambda.1se)),
  error = with(lasso_cv, c(cvm[which(lambda == lasso_cv$lambda.min)], cvm[which(lambda == lasso_cv$lambda.1se)])),  
  p = apply(coef(lasso_fit, s = c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 2, parm) 
) 


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
Box.test(res^2, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

SS.res = sum((y_train - x_train %*% beta_hat)^2)
SS.tot = sum((y_train - mean(y_train))^2)
n = length(y_train)
p = parm(beta_hat)
R.sqrd = 1 - (SS.res / SS.tot)
adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1)) 
adj.R.sqrt * 100

# Koefficienter -----------------------------------------------------------
coef_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(coef_hat != 0) 
coef_hat[idx_hat,]     


test = lm(y_train ~ 0 +x_train[,idx_hat])
summary(test)
