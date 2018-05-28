source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

lasso_cv = cv.lars(x_train, y_train, type = "lasso", intercept = FALSE, 
                   normalize = FALSE, trace = FALSE)

lasso_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, intercept = FALSE)
getmin = getmin_l(lasso_cv$index, lasso_cv$cv, lasso_cv$cv.error)

data.frame(
  lambda = c("min", "1se"), 
  vaedi = c(getmin$lambda.min, getmin$lambda.1se),
  error = c(lasso_cv$cv[getmin$idx_min], lasso_cv$cv[getmin$idx_1se]),
  p = c(parm(coef(lasso_fit, s = getmin$lambda.min, mode = "fraction")), 
        parm(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)

beta_hat = as.vector(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction"))
which(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction") != 0)
fit = x_train %*% beta_hat

# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(tmp$y)
#hist <- histogdens.plot(tmp$y)
#resid <- residuals.plot(tmp$y)
#acf <- residuals.acf.plot(tmp$y)
#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                  layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

adj.r.2_1sd = adj.r.2(y_train, x_train, beta_hat)

beta_hat_min = as.vector(coef(lasso_fit, s = getmin$lambda.min, mode = "fraction"))
adj.r.2_min = adj.r.2(y_train, x_train, beta_hat_min)


# Koefficienter -----------------------------------------------------------
b_hat = coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")
idx_hat = which(b_hat != 0) 
b_hat[idx_hat ]

# Krydsvalidering ---------------------------------------------------------
df_la = data.frame(lasso_cv$index, lasso_cv$cv, lasso_cv$cv.error)

lasso = ggplot(df_la, aes(df_la$lasso_cv.index,df_la$lasso_cv.cv)) + 
  geom_errorbar(aes(ymin = df_la$lasso_cv.cv + df_la$lasso_cv.cv.error, 
                    ymax = df_la$lasso_cv.cv - df_la$lasso_cv.cv.error, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(f==abs(beta)/max(abs(beta))), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= getmin$lambda.min, col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= getmin$lambda.1se, col = "brown"), linetype="dotted") +
  ggtitle("LARS med lasso modifikation") + scale_color_manual(labels = c(expression(f[min]), expression(f[1][sd])), values = c("blue", "brown"))

#grid.arrange(lars, lasso, ncol = 2)


