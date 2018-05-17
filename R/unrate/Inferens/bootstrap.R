source("../scripts/setup_data.R")
library(tidyverse)
set.seed(1)

library(glmnet)
library(gglasso)
library(ggplot2)
library(gridExtra)

n_bootstrap <- 1000
unrate_n <- nrow(X.train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), simplify = FALSE)


# lasso -------------------------------------------------------------------

lasso.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
lasso.best_lambda <- cv.lasso$lambda.1se

# For de indkluderede varible har vi følgende estimater
lasso.beta_hat <- coef(lasso.model, s = lasso.best_lambda)
lasso.idx_hat <- which(lasso.beta_hat != 0)

lasso_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx,], y.train[idx], alpha = 1, lambda = lasso.best_lambda,
                        standardize = FALSE, intercept = FALSE)))
})

lasso_boot_hat <- t(lasso_boot[lasso.idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(lasso.beta_hat)[lasso.idx_hat])

lasso1 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(lasso.beta_hat)[lasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab("Koefficienter") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))

lasso2 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(lasso.beta_hat)[lasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))

# Elastisk net ------------------------------------------------------------

EN.model <- glmnet(X.train, y.train, alpha=0.89, standardize = FALSE, intercept = FALSE)
cv.EN <- cv.glmnet(X.train, y.train, alpha=0.89, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
EN.best_lambda <- cv.EN$lambda.1se

# For de indkluderede varible har vi følgende estimater
EN.beta_hat <- coef(EN.model, s = EN.best_lambda)
EN.idx_hat <- which(EN.beta_hat != 0)

EN_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx,], y.train[idx], alpha = 0.89, lambda = EN.best_lambda,
                        standardize = FALSE, intercept = FALSE)))
})

EN_boot_hat <- t(EN_boot[EN.idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(EN.beta_hat)[EN.idx_hat])

EN1 <- EN_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(EN.beta_hat)[EN.idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))

EN2 <- EN_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(EN.beta_hat)[EN.idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))


lasso.plot <- grid.arrange(lasso1, lasso2, ncol = 2, top = "Lasso")
EN.plot <- grid.arrange(EN1, EN2, ncol = 2, top = "Elastisk net")
grid.arrange(lasso.plot, EN.plot, ncol = 1)


# group lasso -------------------------------------------------------------

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4),
         rep(2,4)) # de laggede værdier tilhører gruppe 2 som unrate 

gglasso.model <- gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(gglasso.model)
cv.gglasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.gglasso)
gglasso.best_lambda <- cv.gglasso$lambda.1se

# For de indkluderede varible har vi følgende estimater
gglasso.beta_hat <- coef(gglasso.model, s = gglasso.best_lambda)
gglasso.idx_hat <- which(gglasso.beta_hat != 0)

gglasso_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(gglasso(X.train[idx, ], y.train[idx], group = grp, intercept = FALSE, lambda = gglasso.best_lambda)))
})

gglasso_boot_hat <- t(gglasso_boot[gglasso.idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(gglasso.beta_hat)[gglasso.idx_hat])

gglasso1 <- gglasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(gglasso.beta_hat)[gglasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))

gglasso2 <- gglasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(gglasso.beta_hat)[gglasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))

grid.arrange(gglasso1, gglasso2, ncol = 2, top = "Group lasso")

# adaptive lasso ----------------------------------------------------------

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

# For de indkluderede varible har vi følgende estimater
alasso.OLS.beta_hat <- coef(alasso.OLS.model, s = alasso.OLS.best_lambda)
alasso.OLS.idx_hat <- which(alasso.OLS.beta_hat != 0)

alasso.OLS_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx,], y.train[idx], alpha = 1, lambda = alasso.OLS.best_lambda,
                        penalty.factor = 1 / abs(OLS_coef), standardize = FALSE, intercept = FALSE)))
})

alasso.OLS_boot_hat <- t(alasso.OLS_boot[alasso.OLS.idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(alasso.OLS.beta_hat)[alasso.OLS.idx_hat])

alasso.OLS1 <- alasso.OLS_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(alasso.OLS.beta_hat)[alasso.OLS.idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))

alasso.OLS2 <- alasso.OLS_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(alasso.OLS.beta_hat)[alasso.OLS.idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))


# lasso vægte
lasso.beta_hat_test = as.vector(lasso.beta_hat) %>% .[-1]
lasso.idx_hat <- which(lasso.beta_hat_test != 0)
lasso_coef <- lasso.beta_hat_test[lasso.idx_hat]

alasso.lasso.model <- glmnet(X.train[,lasso.idx_hat], y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                           penalty.factor = 1 / abs(lasso_coef))
cv.alasso.lasso <- cv.glmnet(X.train[,lasso.idx_hat], y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(lasso_coef))
plot(cv.alasso.lasso)
alasso.lasso.best_lambda <- cv.alasso.lasso$lambda.min

# For de indkluderede varible har vi følgende estimater
alasso.lasso.beta_hat <- coef(alasso.lasso.model, s = alasso.lasso.best_lambda)
alasso.lasso.idx_hat <- which(alasso.lasso.beta_hat != 0)

alasso.lasso_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx,], y.train[idx], alpha = 1, lambda = alasso.lasso.best_lambda,
                        penalty.factor = 1 / abs(lasso_coef), standardize = FALSE, intercept = FALSE)))
})

alasso.lasso_boot_hat <- t(alasso.lasso_boot[alasso.lasso.idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(alasso.lasso.beta_hat)[alasso.lasso.idx_hat])

alasso.lasso1 <- alasso.lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(alasso.lasso.beta_hat)[alasso.lasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))

alasso.lasso2 <- alasso.lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(alasso.lasso.beta_hat)[alasso.lasso.idx_hat])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))

alasso.OLS.plot <- grid.arrange(alasso.OLS1, alasso.OLS2, ncol = 2, top = "Adaptive lasso med OLS vægte")
alasso.lasso.plot <- grid.arrange(alasso.lasso1, alasso.lasso2, ncol = 2, top = "Adaptive lasso med lasso vægte")
grid.arrange(alasso.OLS.plot, alasso.lasso.plot, ncol = 1)


