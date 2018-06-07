source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
source("shrinkage_metoder/res_plot.R")

set.seed(1)
library(selectiveInference)

fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)

n <- length(y_train)
lambda <- fit_bic_lasso$lambda * n
beta = coef(fit_lasso, s = lambda/n, exact = TRUE, x = x_train, y = y_train)[-1]
out_glm <- fixedLassoInf(x_train, y_train, beta, lambda, intercept = FALSE, alpha = 0.1)

idx = which(beta !=0 )
colnames(x_train[,idx])


beta_inf = out_glm$coef0
fit = x_train[,idx] %*% beta_inf 


# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
tmp = data.frame(Date = as.Date(dato_train), y = res)

qqnorm<- qqnorm.plot(res)
hist <- histogdens.plot(res)
resid <- residuals.plot(res)
acf <- residuals.acf.plot(res)
print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")
