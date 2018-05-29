source("package.R")
source("data_unrate.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/bic_lars.R")

lars_fit = lars(x_train, y_train, type = "lar", normalize = FALSE, intercept = FALSE)
lars_bic = larsBIC(y_train, x_train,lars_fit )

coef = coef(lars_fit, s = 21, mode = "step")
idx_hat = which(coef != 0)
coef[idx_hat]

coef_1 = coef(lars_fit, s = lars_bic$f_hat, mode = "fraction")
idx_hat_1 = which(coef_1 != 0)
coef_1[idx_hat_1]




# residualer --------------------------------------------------------------
tmp = data.frame(Date = as.Date(dato_train), y = lars_bic$scale_res)

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


# adj R^2 -----------------------------------------------------------------

coef_min = coef(lars_fit, s = lars_bic$f_hat, mode = "fraction")
idx_min = which(coef_min != 0)
lm_min = lm(y_train~0 + x_train[, (idx_min)])
summary(lm_min)
logLik(lm_min)
