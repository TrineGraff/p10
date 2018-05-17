source("../scripts/setup_data.R")

library(selectiveInference)
set.seed(1)

## lasso
lasso.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
lasso.best_lambda <- cv.lasso$lambda.1se

n <- length(y.train)
lambda <- lasso.best_lambda * n
beta = coef(lasso.model, s = lambda/n)[-1]

out_glm <- fixedLassoInf(X.train, y.train, beta, lambda, intercept = FALSE, alpha = 0.1)
