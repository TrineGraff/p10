source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")


# lasso -------------------------------------------------------------------
getmin_l = getmin(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

data.frame(
  lambda = c("min", "1se"), 
  vaedi = c(getmin_l$lambda.min, getmin_l$lambda.1se),
  error = c(lars_cv$cv[getmin_l$idx_min], lars_cv$cv[getmin_l$idx_1se]),
  p = c(parm(coef(lars_, s = getmin_l$lambda.min, mode = "fraction")), 
        parm(coef(lars_, s = getmin_l$lambda.1se, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)


beta_hat = coef(lasso_fit, s = getmin$lambda.min, mode = "lambda")


which(beta_hat != 0)
predict(lars_, s = getmin_l$lambda.min, mode = "fraction", type = "coefficients")$coefficients
coef.lars(fit.lasso,s=0.5,mode="lambda")

# plot --------------------------------------------------------------------

df_la = data.frame(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

ggplot(df_la, aes(df_la$lars_cv.index,df_la$lars_cv.cv)) + 
  geom_errorbar(aes(ymin = df_la$lars_cv.cv + df_la$lars_cv.cv.error, 
                    ymax = df_la$lars_cv.cv - df_la$lars_cv.cv.error, width = .1)) +
  geom_point(col = "red") +
  labs(x = "Fraktion af sidste L1 norm", y = "MSE", color = "") + 
  geom_vline(aes(xintercept= l1_min, col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= l1_1se, col = "brown"), linetype="dotted") +
  ggtitle("Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


# Elastic net -------------------------------------------------------------

#hvilken lambda_2 har den mindste krydsvalideringsfejl. 
min_0.01 = min(fit0.01$cv)
min_0.1 = min(fit0.1$cv)
min_1 = min(fit1$cv)
min_10 = min(fit10$cv)
min_100 = min(fit100$cv)
min = which.min(c(min_0.01, min_0.1, min_1, min_10, min_100))

#finder det s med mindst krydsvaliderings fejl
getmin_en = getmin(fit0.01$s, fit0.01$cv, fit0.01$cv.error)

enet = enet(x, y, lambda = 0.01, intercept= FALSE, normalize = FALSE)


# adaptive lasso med ols ----------------------------------------------------------

fit.ols = lars(x_scale, y, type="lasso", normalize=FALSE, intercept = FALSE)

lambda.ols = getmin(cv.ols$index, cv.ols$cv, cv.ols$cv.error)
coef(fit.ols, s = lambda$lambda.min)

predict(fit.ols, s = lambda$lambda.min, mode = "fraction", type = "coefficients")$coefficients
beta_hat = coef(fit.ols, s = lambda$lambda.min, mode = "fraction")
length(which(beta_hat != 0))


# adaptive lasso med lasso vægte ----------------------------------------------------------

adap_lasso_fit = lars(x_scale_las, y, intercept = FALSE, type = "lasso", normalize = FALSE)

lambda.adap.l = getmin(cv.adap.l$index, cv.adap.l$cv, cv.adap.l$cv.error)
predict(adap_lasso_fit, s = lambda.adap.l$lambda.min, mode = "fraction", type = "coefficients")$coefficients



