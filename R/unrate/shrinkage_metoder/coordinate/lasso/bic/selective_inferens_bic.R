source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/coordinate/bic.R")
set.seed(1)
library(selectiveInference)

fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)

n <- length(y_train)
lambda <- fit_bic_lasso$lambda * n
beta = coef(fit_lasso, s = lambda/n, exact = TRUE, x = x_train, y = y_train)[-1]
?fixedLassoInf
out_glm <- fixedLassoInf(x_train, y_train, beta, lambda, intercept = FALSE, alpha = 0.1)

idx = which(beta !=0 )
colnames(x_train[,idx])
