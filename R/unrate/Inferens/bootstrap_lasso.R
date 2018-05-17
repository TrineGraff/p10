source("../scripts/setup_data.R")
library(tidyverse)
set.seed(1)

library(glmnet)
library(ggplot2)
library(gridExtra)

### lasso
lasso.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)

# Vi vælger lambda.1se som værdien af lambda
best_lambda <- cv.lasso$lambda.1se

# For de indkluderede varible har vi følgende estimater
beta_hat <- coef(lasso.model, s = best_lambda)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ] # 14 variable


# bootstrap ---------------------------------------------------------------

# Vha bootstrap metoden, vil vi quantificere variation af beta_hat.
# Vi resampler datasættet 1000 gange og udtrækker koefficienterne for hver realization
# Vi slicer bootstrap data til kun at indeholde de valgte variable

# Bootstrapping 
n_bootstrap <- 1000
unrate_n <- nrow(X.train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), simplify = FALSE)

unrate_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx,], y.train[idx], alpha = 1, lambda = best_lambda,
                        standardize = FALSE, intercept = FALSE)))
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
  theme(plot.margin=unit(c(0,0,1,0),"inches")) 

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
  theme(plot.margin=unit(c(0,0,1,0),"inches")) 

grid.arrange(gg1, gg2, ncol = 2)


