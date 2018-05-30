source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

lars_cv = cv.lars(x_train, y_train, type = "lar", intercept = FALSE, 
                   normalize = FALSE, trace = TRUE)
lars_fit = lars(x_train, y_train, type = "lar", intercept = FALSE, 
                normalize = FALSE)
getmin_lars = getmin_l(lars_cv$index, lars_cv$cv, lars_cv$cv.error)


# Værdierne ---------------------------------------------------------------
## tester om de er de samme for en standard afvigelse (det er det)
test = coef(lars_fit, s = getmin_lars$lambda.1se, mode = "step")
idx = which(test != 0)
test[idx]

#omregner til fraction
s1 = apply(abs(lars_fit$beta), 1, sum) #summere over rækken. dvs vi får summen af koefficienterne
f = s1/max(s1)

test_1 = coef(lars_fit, s= f[20], mode = "fraction")
idx_1 = which(test_1 != 0)
test_1[idx_1]

test_2 = coef(lars_fit, s = lars_fit$lambda[20], mode = "lambda")
idx_2 = which(test_2 != 0)
test_2[idx_2]

## tester om det er de samme med minimum

test = coef(lars_fit, s = getmin_lars$lambda.min, mode = "step")
idx = which(test != 0)
test[idx]

test_1 = coef(lars_fit, s= f[28], mode = "fraction")
idx_1 = which(test_1 != 0)
test_1[idx_1]

test_2 = coef(lars_fit, s = lars_fit$lambda[28], mode = "lambda")
idx_2 = which(test_2 != 0)
test_2[idx_2]

## får de samme resultater

data.frame(
  s = c("min", "1se"), 
  vaedi = c(f[28], f[20]),
  error = c(lars_cv$cv[getmin_lars$idx_min], lars_cv$cv[getmin_lars$idx_1se]),
  p = c(parm(coef(lars_fit, s = f[20], mode = "fraction")), 
        parm(coef(lars_fit, s = f[28], mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)

beta_hat = as.vector(coef(lars_fit, s = f[20], mode = "fraction"))
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
#                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")

# Adj. R ------------------------------------------------------------------

coef_min = coef(lars_fit, s = f[20], mode = "fraction")
idx_min = which(coef_min != 0)
lm_min = lm(y_train~0 + x_train[, (idx_min)])
summary(lm_min)
logLik(lm_min)

coef_1sd = coef(lars_fit, s = f[28], mode = "fraction")
idx_1sd = which(coef_1sd != 0)
lm_1sd = lm(y_train~0 + x_train[, (idx_1sd - 1)])
summary(lm_1sd)
logLik(lm_1sd)

# Koefficienter -----------------------------------------------------------
b_hat = coef(lars_fit, s = getmin_lars$lambda.1se, mode = "step")
idx_hat = which(b_hat != 0) 
b_hat[idx_hat ]

# Krydsvalidering ---------------------------------------------------------
df_lars = data.frame(f = f, cv = lars_cv$cv, cv_error = lars_cv$cv.error)

lars = ggplot(df_lars, aes(df_lars$f, df_lars$cv)) + 
  geom_errorbar(aes(ymin = df_lars$cv + df_lars$cv_error, 
                    ymax = df_lars$cv - df_lars$cv_error, width = .1))+
  geom_point(col = "red") +
  labs(x = expression(f==abs(beta)/max(abs(beta))), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= df_lars$f[28], col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= df_lars$f[20], col = "brown"), linetype="dotted") +
  ggtitle("LARS") + scale_color_manual(labels = c(expression(f[min]), expression(f[1][sd])), values = c("blue", "brown"))

