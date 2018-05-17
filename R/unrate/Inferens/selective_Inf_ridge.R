source("../scripts/setup_data.R")

library(selectiveInference)
set.seed(1)

## ridge regression
ridge.model <- glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(cv.ridge)
ridge.best_lambda <- cv.ridge$lambda.min

n <- length(y.train)
lambda <- ridge.best_lambda * n
beta = coef(ridge.model, s = lambda/n)[-1]

out_glm <- fixedLassoInf(X.train, y.train, beta, lambda, intercept = FALSE, alpha = 0.1)
# fejl