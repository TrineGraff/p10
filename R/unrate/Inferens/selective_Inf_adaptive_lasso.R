source("../scripts/setup_data.R")

library(selectiveInference)
set.seed(1)

n <- length(y.train)

## adaptive lasso

# OLS vægte
df.OLS.X.train <- data.frame(X.train)
lm.model <- lm(y.train ~. -1, data = df.OLS.X.train)
OLS_coef <- coef(lm.model)
alasso.OLS.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                           penalty.factor = 1 / abs(OLS_coef))
cv.alasso.OLS <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(OLS_coef))
plot(cv.alasso.OLS)
alasso.OLS.best_lambda <- cv.alasso.OLS$lambda.min
lambda.OLS <- alasso.OLS.best_lambda * n
beta.OLS = coef(alasso.OLS.model, s = lambda.OLS/n, exact = TRUE, x = X.train, y = y.train)[-1]

out_glm.OLS <- fixedLassoInf(X.train, y.train, beta.OLS, lambda.OLS, intercept = FALSE, alpha = 0.1)


# ridge vægte
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
ridge.best_lambda <- cv.ridge$lambda.min
ridge.model <- glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE, 
                      lambda = ridge.best_lambda)

ridge_coef <- as.numeric(coef(cv.ridge, s = ridge.best_lambda))
cv.alasso.ridge <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                             penalty.factor = 1 / abs(ridge_coef))
plot(cv.alasso.ridge)
alasso.ridge.best_lambda <- cv.alasso.ridge$lambda.min
alasso.ridge.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                             penalty.factor = 1 / abs(ridge_coef), lambda = alasso.ridge.best_lambda)

lambda.ridge <- alasso.ridge.best_lambda * n
beta.ridge = coef(alasso.ridge.model, s = lambda.ridge/n, exact = TRUE, x = X.train, y = y.train)[-1]

out_glm.ridge <- fixedLassoInf(X.train, y.train, beta.ridge, lambda.ridge, intercept = FALSE, alpha = 0.1)
