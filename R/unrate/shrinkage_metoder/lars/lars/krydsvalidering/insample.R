source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
set.seed(1)

lars_cv = cv.lars(x_train, y_train, type = "lar", intercept = FALSE, 
                   normalize = FALSE, trace = TRUE)
lars_fit = lars(x_train, y_train, type = "lar", intercept = FALSE, 
                normalize = FALSE)
getmin_lars = getmin_l(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

data.frame(
  s = c("min", "1se"), 
  vaedi = c(getmin_lars$lambda.min, getmin_lars$lambda.1se),
  error = c(lars_cv$cv[getmin_lars$idx_min], lars_cv$cv[getmin_lars$idx_1se]),
  p = c(parm(coef(lars_fit, s = getmin_lars$lambda.min, mode = "step")), 
        parm(coef(lars_fit, s = getmin_lars$lambda.1se, mode = "step")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)

beta_hat = as.vector(coef(lars_fit, s = getmin_lars$lambda.min, mode = "step"))
fit = x_train %*% beta_hat

# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
#tmp = data.frame(Date = as.Date(dato_train), y = res)

#qqnorm<- qqnorm.plot(res)
#hist <- histogdens.plot(res)
#resid <- residuals.plot(res)
#acf <- residuals.acf.plot(res)
#print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
#                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")
Box.test(res^2, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

SS.res = sum((y_train - x_train %*% beta_hat)^2)
SS.tot = sum((y_train - mean(y_train))^2)
n = length(y_train)
p = parm(beta_hat)
R.sqrd = 1 - (SS.res / SS.tot)
adj.R.sqrt = 1 - (1 - R.sqrd) * ((n - 1) / (n - p - 1)) 
adj.R.sqrt * 100

# Koefficienter -----------------------------------------------------------
b_hat = coef(lars_fit, s = getmin_lars$lambda.1se, mode = "step")
idx_hat = which(b_hat != 0) 
b_hat[idx_hat ]

# Krydsvalidering ---------------------------------------------------------
df_lars = data.frame(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

lars = ggplot(df_lars, aes(df_lars$lars_cv.index,df_lars$lars_cv.cv)) + 
  geom_errorbar(aes(ymin = df_lars $lars_cv.cv + df_lars $lars_cv.cv.error, 
                    ymax = df_lars $lars_cv.cv - df_lars $lars_cv.cv.error, width = .1)) +
  geom_point(col = "red") +
  labs(x = "Antallet af steps", y = "MSE", color = "") + 
  geom_vline(aes(xintercept= getmin_lars$lambda.min, col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= getmin_lars$lambda.1se, col = "brown"), linetype="dotted") +
  ggtitle("LARS") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


