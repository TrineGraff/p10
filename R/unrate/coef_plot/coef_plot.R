source("../scripts/setup_data.R")
set.seed(1)

library(glmnet)
library(ggplot2)
library(gridExtra)
library(data.table)


# lasso
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
best_lambda <- cv.lasso$lambda.1se
lasso_coef <- cv.lasso$glmnet.fit$beta[, cv.lasso$glmnet.fit$lambda == best_lambda]
lasso_coef[lasso_coef != 0]

## ridge regression
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(cv.ridge)
best_lambda <- cv.ridge$lambda.min
ridge_coef <- cv.ridge$glmnet.fit$beta[, cv.ridge$glmnet.fit$lambda == best_lambda]

## elastisk net
cv.EN <- cv.glmnet(X.train, y.train, alpha = 0.89, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
best_lambda <- cv.EN$lambda.1se
EN_coef <- cv.EN$glmnet.fit$beta[, cv.EN$glmnet.fit$lambda == best_lambda]
EN_coef[EN_coef != 0]

## group lasso
library(gglasso)
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2,4)) 
# lag 1 - lag 4 tilhører gruppe 2 som unrate
cv.group_lasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.group_lasso)
best_lambda <- cv.group_lasso$lambda.1se
group_lasso_coef <- cv.group_lasso$gglasso.fit$beta[, cv.group_lasso$gglasso.fit$lambda == best_lambda] 

## adaptive lasso
# OLS vægte
df.OLS.X.train <- data.frame(X.train)
lm.model <- lm(y.train ~. -1, data = df.OLS.X.train)
OLS_coef <- coef(lm.model)
cv.alasso.OLS <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(OLS_coef))
plot(cv.alasso.OLS)
best_lambda <- cv.alasso.OLS$lambda.min
alasso.OLS_coef <- cv.alasso.OLS$glmnet.fit$beta[, cv.alasso.OLS$glmnet.fit$lambda == best_lambda]

# lasso vægte
cv.alasso.ridge <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                             penalty.factor = 1 / abs(ridge_coef))
plot(cv.alasso.ridge)
best_lambda <- cv.alasso.ridge$lambda.1se
alasso.ridge_coef <- cv.alasso.ridge$glmnet.fit$beta[, cv.alasso.ridge$glmnet.fit$lambda == best_lambda]
alasso.ridge_coef[alasso.ridge_coef != 0]

# plots -------------------------------------------------------------------
coef = data.table(Lasso = lasso_coef,
                  Elastisk_net = EN_coef,
                  Adaptive_lasso_OLS = alasso.OLS_coef,
                  Adaptive_lasso_ridge = alasso.ridge_coef)
coef[, feature := names(ridge_coef)]
coef[, group := grp]

color(coef$feature) = 3


names(EN_coef[EN_coef != 0])

koef <- coef[feature == "DPCERA3M086SBEA" | feature == "IPDMAT" | feature == "HWIURATIO" | feature == "CLF16OV"
               | feature == "CE16OV" | feature == "UEMPLT5" | feature == "UEMP5TO14" | feature == "UEMP15OV"
               | feature == "CLAIMSx" | feature == "PAYEMS"  | feature == "USCONS" | feature == "USTRADE"
               | feature == "T1YFFM" | feature == "T5YFFM" | feature == "T10YFFM" | feature == "AAAFFM"
               | feature == "AMDMNOx" | feature == "TB6MS" | feature == "GS5" | feature == "EXUSUKx"
               | feature == "CPIMEDSL" | feature == "lag 1" | feature == "lag 4"]


to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")

gg1 <- ggplot(to_plot, aes(x = feature, y = coefficient, fill = variable)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) 
