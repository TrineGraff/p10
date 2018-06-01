source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

beta_test = as.vector(coef(fit_lasso, s = fit_bic_lasso$lambda)) %>% .[-1]
idx_beta = which(beta_hat != 0)
v_l = 1/abs(beta_hat[idx_beta])^0.5 

adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso_bic = lassoBIC(y_train, x_train[,idx_beta], adap_lasso_fit)

# residualer --------------------------------------------------------------
tmp = data.frame(Date = as.Date(dato_train), y = adap_lasso_bic$scale_res)

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


# Adjusted R^2 ------------------------------------------------------------


x_ny = x_train[,idx_beta]
coef_bic = coef(adap_lasso_fit, s = adap_lasso_bic$lambda)
idx_bic = which(coef_bic != 0)
lm_bic = lm(y_train~0+ x_ny[, idx_bic -1])
summary(lm_bic)
logLik(lm_bic)

