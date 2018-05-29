source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 


gglasso_fit = gglasso(x_train, y_train, group = grp, intercept = FALSE, loss = "ls")

grp_bic = lassoBIC(y_train, x_train, gglasso_fit)

# residualer --------------------------------------------------------------
tmp = data.frame(Date = as.Date(dato_train), y = grp_bic$scale_res)

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


# Koefficienter -----------------------------------------------------------

coef_bic= coef(gglasso_fit, s = grp_bic$lambda)
idx_bic = which(coef_bic != 0)

lm = lm(y_train ~ 0 + x_train[,idx_bic -1 ])
summary(lm)
logLik(lm)
