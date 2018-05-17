source("../scripts/setup_data.R")

library(selectiveInference)
set.seed(1)

## EN
EN.model <- glmnet(X.train, y.train, alpha = 0.5, standardize = FALSE, intercept = FALSE)
cv.EN <- cv.glmnet(X.train, y.train, alpha = 0.5, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
EN.best_lambda <- cv.EN$lambda.1se

n <- length(y.train)
lambda <- EN.best_lambda * n
beta = coef(EN.model, s = lambda/n)[-1]

out_glm <- fixedLassoInf(X.train, y.train, beta, lambda, intercept = FALSE, alpha = 0.1)
# alle får en p-værdi på 1