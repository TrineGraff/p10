source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/adj.r.2.R")

set.seed(1)

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 


gglasso_cv <- cv.gglasso(x_train, y_train, group = grp, nfold = 10, intercept = FALSE, loss = "ls" )
gglasso_fit = gglasso(x_train, y_train, group = grp, intercept = FALSE, loss = "ls", dfmax = 8)


data.frame(c(1,grp),coef(gglasso_fit, s = gglasso_cv$lambda.1se))



plot(gglasso_fit, xlim = c(-7, -5), ylim = c(-0.001, 0.001))

summary(gglasso_fit)
data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(gglasso_cv$lambda.min), log(gglasso_cv$lambda.1se)),
  error = with(gglasso_cv, c(cvm[which(lambda == gglasso_cv$lambda.min)], cvm[which(lambda == gglasso_cv$lambda.1se)])),  
  p = apply(coef(gglasso_fit , s = c(gglasso_cv$lambda.min, gglasso_cv$lambda.1se)), 2, parm) 
) 

beta_hat = as.vector(coef(gglasso_fit, s = gglasso_cv$lambda.1se)) %>% .[-1] #fjerner skæringen

# residualer --------------------------------------------------------------
fit = x_train %*% beta_hat
res = y_train - fit 
res = scale(res)

tmp = data.frame(Date = as.Date(dato_train), y = res)

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

adj.r.2_1sd = adj.r.2(y_train, x_train, beta_hat )

coef_min = as.vector(coef(gglasso_fit, s = gglasso_cv$lambda.min)) %>% .[-1]
adj.r.2(y_train, x_train, coef_min)


# Koefficienter -----------------------------------------------------------

coef_hat = coef(gglasso_fit, s = gglasso_cv$lambda.1se)
idx_hat = which(coef_hat == 0) 
coef_hat[idx_hat, ] #dem der er lig nul

