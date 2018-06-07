source("setup_data.R")
library(tidyverse)
library(gglasso)
library(MCS)

## AR(4)
AR4 <- ar(y.train, bic = TRUE, order.max = 4, method = "ols")
AR4.predict <- X.test %*% AR4$asy.se.coef$ar


## Faktor modellen

## ridge regression
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(cv.ridge)
ridge.best_lambda <- cv.ridge$lambda.min
ridge.model <- glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE, 
                      lambda = ridge.best_lambda)

## lasso
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
lasso.best_lambda <- cv.lasso$lambda.1se
lasso.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                      lambda = lasso.best_lambda)

## EN
cv.EN <- cv.glmnet(X.train, y.train, alpha=0.5, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
EN.best_lambda <- cv.EN$lambda.1se
EN.model <- glmnet(X.train, y.train, alpha=0.5, standardize = FALSE, intercept = FALSE,
                   lambda = EN.best_lambda)

## group lasso
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4),
         rep(2,4)) # de laggede værdier tilhører gruppe 2 som unrate 
cv.gglasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.gglasso)
gglasso.best_lambda <- cv.gglasso$lambda.1se
gglasso.model <- gglasso(X.train, y.train, group = grp, intercept = FALSE,
                         lambda = gglasso.best_lambda)

## adaptive lasso
df.OLS.X.train <- data.frame(X.train)
lm.model <- lm(y.train ~. -1, data = df.OLS.X.train)
OLS_coef <- coef(lm.model)
cv.alasso.OLS <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(OLS_coef))
plot(cv.alasso.OLS)
alasso.OLS.best_lambda <- cv.alasso.OLS$lambda.min
alasso.OLS.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                           penalty.factor = 1 / abs(OLS_coef), lambda = alasso.OLS.best_lambda)


ridge_coef <- as.numeric(coef(cv.ridge, s = ridge.best_lambda))
cv.alasso.ridge <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                                 penalty.factor = 1 / abs(ridge_coef))
plot(cv.alasso.ridge)
alasso.ridge.best_lambda <- cv.alasso.ridge$lambda.min
alasso.ridge.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                             penalty.factor = 1 / abs(ridge_coef), lambda = alasso.ridge.best_lambda)

# Prædiktion
ridge.pred <- predict(ridge.model, s = ridge.best_lambda, newx = X.test)
lasso.pred <- predict(lasso.model, s = lasso.best_lambda, newx = X.test)
EN.pred <- predict(EN.model, s = EN.best_lambda, newx = X.test)
gglasso.pred <- predict(gglasso.model, s = gglasso.best_lambda, newx = X.test)
alasso.OLS.pred <- predict(alasso.OLS.model, s = alasso.OLS.best_lambda, newx = X.test)
alasso.ridge.pred <- predict(alasso.ridge.model, s = alasso.ridge.best_lambda, newx = X.test)

diffs <- data.frame(ridge.pred - y.test,
                lasso.pred - y.test,
                EN.pred - y.test,
                gglasso.pred - y.test,
                alasso.OLS.pred - y.test,
                alasso.ridge.pred - y.test)
colnames(diffs) <- c("ridge regressin","lasso","EN", "group lasso",
                     "adaptive lasso med OLS vægte",
                     "adaptive lasso med ridge vægte")

# Tabs tabeller for MCS
loss <- list(
  abs = abs(diffs),
  sq = (diffs)^2
)

res <- setNames(vector("list", length(loss)), names(loss))
for (ln in names(loss)) {
  MCS <- MCSprocedure(Loss = diffs, alpha = 0.2, B = 5000, statistic = 'Tmax')
  res[[ln]] <- MCS
}

saveRDS(res, file = "mcs80.rds")

