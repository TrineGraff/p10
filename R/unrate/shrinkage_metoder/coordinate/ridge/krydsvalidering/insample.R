source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

ridge_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 0, intercept = FALSE, standardize=FALSE)
ridge_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 0, standardize=FALSE)

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(ridge_cv$lambda.min), log(ridge_cv$lambda.1se)),
  error = with(ridge_cv, c(cvm[which(lambda == ridge_cv$lambda.min)], cvm[which(lambda == ridge_cv$lambda.1se)])),  
  p = apply(coef(ridge_fit, s = c(ridge_cv$lambda.min, ridge_cv$lambda.1se)), 2, parm) 
) 

beta_hat = as.vector(coef(ridge_fit, s = ridge_cv$lambda.min)) %>% .[-1] #fjerner skæringen

# residualer --------------------------------------------------------------
fit = x_train %*% beta_hat 
res =  y_train - fit 
res = scale(res)

tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(res)
#hist <- histogdens.plot(res)
#resid <- residuals.plot(res)
#acf <- residuals.acf.plot(res)

#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))


skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")
Box.test(res^2, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

adj.r.2_min = adj.r.2(y_train, x_train, beta_hat )

coef_1sd = as.vector(coef(ridge_fit, s = ridge_cv$lambda.min)) %>% .[-1]
adj.r.2(y_train, x_train, coef_1sd )

