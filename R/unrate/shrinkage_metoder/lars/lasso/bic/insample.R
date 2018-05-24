source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/bic_lars.R")

lasso_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, 
                 intercept = FALSE)
lasso_bic = lassoBIC(y_train, x_train, lasso_fit)

coef = coef(lasso_fit, s = lasso_bic$f_hat, mode = "fraction")
which(coef != 0)

# residualer --------------------------------------------------------------

tmp = data.frame(Date = as.Date(dato_train), y = lasso_bic$scale_res)

qqnorm<- qqnorm.plot(tmp$y)
hist <- histogdens.plot(tmp$y)
resid <- residuals.plot(tmp$y)
acf <- residuals.acf.plot(tmp$y)
print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(tmp$y)
kurtosis(tmp$y)
jarque.bera.test(tmp$y)
Box.test(tmp$y, lag = 10, "Ljung-Box")
Box.test(tmp$y^2, lag = 10, "Ljung-Box")