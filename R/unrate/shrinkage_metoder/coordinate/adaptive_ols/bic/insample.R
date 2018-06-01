source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v_2 = 1/abs(coef$`fit_ols$coefficients`)^2

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                        standardize=FALSE, penalty.factor = v_2)
adap_ols_bic = lassoBIC(y_train, x_train, adap_ols_fit)


# residualer --------------------------------------------------------------

tmp = data.frame(Date = as.Date(dato_train), y = adap_ols_bic$scale_res)

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


# Adjusted R^2 ------------------------------------------------------------

coef_bic = coef(adap_ols_fit, s = adap_ols_bic$lambda)
idx_bic = which(coef_bic != 0)
lm_bic = lm(y_train~0+x_train[, (idx_bic - 1)])
summary(lm_bic)
logLik(lm_bic)


