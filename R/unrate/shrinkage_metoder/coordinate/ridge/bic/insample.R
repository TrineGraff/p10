source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

fit_ridge = glmnet(x_train, y_train, family = "gaussian", alpha = 0, intercept = FALSE, standardize=FALSE)
fit_bic_ridge = lassoBIC(y_train, x_train, fit_ridge)

# residualer --------------------------------------------------------------
tmp = data.frame(Date = as.Date(dato_train), y = fit_bic_ridge$scale_res)

#qqnorm<- qqnorm.plot(tmp$y)
#hist <- histogdens.plot(tmp$y)
#resid <- residuals.plot(tmp$y)
#acf <- residuals.acf.plot(tmp$y)

#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
 #                  layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))


skewness(tmp$y)
kurtosis(tmp$y)
jarque.bera.test(tmp$y)
Box.test(tmp$y, lag = 10, "Ljung-Box")


# adj. R^2 ----------------------------------------------------------------

coef_bic= coef(fit_ridge, s = fit_bic_ridge$lambda)
idx_bic = which(coef_bic != 0)

lm = lm(y_train ~ 0 + x_train[,idx_bic -1 ])
summary(lm)
logLik(lm)

