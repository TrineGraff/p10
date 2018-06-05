source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/coordinate/bic.R")

fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)

beta_hat = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]

# residualer --------------------------------------------------------------
tmp = data.frame(Date = as.Date(dato_train), y = fit_bic_lasso$scale_res)

#qqnorm<- qqnorm.plot(tmp$y)
#hist <- histogdens.plot(tmp$y)
#resid <- residuals.plot(tmp$y)
#acf <- residuals.acf.plot(tmp$y)

#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))


skewness(tmp$y)
kurtosis(tmp$y)
jarque.bera.test(tmp$y)
Box.test(tmp$y, lag = 10, "Ljung-Box")

# Koefficienterne ---------------------------------------------------------

b_hat = (coef(fit_lasso, s = fit_bic_lasso$lambda))
idx_hat = which(b_hat != 0) 
b_hat[idx_hat,] 


# adj R^2 -----------------------------------------------------------------

coef_bic= coef(fit_lasso, s = fit_bic_lasso$lambda)
idx_bic = which(coef_bic != 0)

lm = lm(y_train ~ 0 + x_train[,idx_bic -1 ])
summary(lm)
logLik(lm)
