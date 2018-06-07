source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

coef_kryds = coef(lasso_fit, s = lasso_cv$lambda.1se)[-1]

n <- length(y_train)
lambda = lasso_cv$lambda.1se * n
fixed_lasso_kryds = fixedLassoInf(x_train, y_train, coef_kryds, lambda = lambda, 
                                  intercept = FALSE, alpha = 0.1)
idx_1sd

beta_inf = fixed_lasso_kryds$coef0
fit = x_train[,idx_1sd -1 ] %*% beta_inf 


# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
tmp = data.frame(Date = as.Date(dato_train), y = res)

qqnorm<- qqnorm.plot(res)
hist <- histogdens.plot(res)
resid <- residuals.plot(res)
acf <- residuals.acf.plot(res)
print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
                  layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")