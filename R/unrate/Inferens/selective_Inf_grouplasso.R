source("../scripts/setup_data.R")

library(gglasso)
library(selectiveInference)
set.seed(1)

## group lasso
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4),
         rep(2,4)) # de laggede værdier tilhører gruppe 2 som unrate 
gglasso.model <- gglasso(X.train, y.train, group = grp, intercept = FALSE)
cv.gglasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.gglasso)
gglasso.best_lambda <- cv.gglasso$lambda.1se

n <- length(y.train)
lambda <- gglasso.best_lambda * n
beta = coef(gglasso.model, s = lambda/n)[-1]

out_glm <- fixedLassoInf(X.train, y.train, beta, lambda, intercept = FALSE, alpha = 0.1)
# fejl