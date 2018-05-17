source("../scripts/setup_data.R")
set.seed(1)

library(glmnet)
library(ggplot2)
library(gridExtra)

### adaptive lasso med ols vægte
df.OLS.X.train <- data.frame(X.train)
lm.model <- lm(y.train ~. -1, data = df.OLS.X.train)
OLS_coef <- coef(lm.model)
alasso.OLS.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                           penalty.factor = 1 / abs(OLS_coef))
cv.alasso.OLS <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(OLS_coef))
plot(cv.alasso.OLS)
best_lambda <- cv.alasso.OLS$lambda.min

# For de indkluderede varible har vi følgende estimater
beta_hat <- coef(alasso.OLS.model, s = best_lambda)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ] # 2 variable


### adaptive lasso med ridge vægte
ridge.model <- glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
best_lambda <- cv.ridge$lambda.min
ridge_coef <- as.numeric(coef(cv.ridge, s = best_lambda))
ridge_coef[ridge_coef!=0] # vælger alle 126 variable

alasso.ridge.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                             penalty.factor = 1 / abs(ridge_coef))
cv.alasso.ridge <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                                 penalty.factor = 1 / abs(ridge_coef))
plot(cv.ridge)
best_lambda <- cv.alasso.ridge$lambda.min

# For de indkluderede varible har vi følgende estimater
beta_hat <- coef(alasso.ridge.model, s = best_lambda)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ] # 7 variable

# bootstrap ---------------------------------------------------------------

# Vha bootstrap metoden, vil vi quantificere variation af beta_hat.
# Vi resampler datasættet 1000 gange og udtrækker koefficienterne for hver realization
# Vi slicer bootstrap data til kun at indeholde de valgte variable

# Bootstrapping 
n_bootstrap <- 1000
unrate_n <- nrow(X.train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), simplify = FALSE)

unrate_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx, ], y.train[idx], alpha = 1, standardize = FALSE, intercept = FALSE, 
                        penalty.factor = 1 / abs(OLS_coef), lambda = best_lambda)))
})


unrate_boot_hat <- t(unrate_boot[idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(beta_hat)[idx_hat])


gg1 <- unrate_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(beta_hat)[idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot() + 
  coord_flip() +
  xlab(" ") +
  ylab("Koefficienter") +
  ggtitle("Bootstrap Samples") +
  theme(plot.margin=unit(c(0,0,2.5,0),"inches")) 

gg2 <- unrate_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(beta_hat)[idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Bootstrap Probability of 0") +
  theme(plot.margin=unit(c(0,0,2.5,0),"inches")) 

grid.arrange(gg1, gg2, ncol = 2)


