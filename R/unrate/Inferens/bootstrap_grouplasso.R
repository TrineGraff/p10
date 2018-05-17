source("../scripts/setup_data.R")
set.seed(1)

library(gglasso)
library(ggplot2)
library(gridExtra)

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4),
         rep(2,4)) # de laggede værdier tilhører gruppe 2 som unrate 

group.lasso.model <- gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(group.lasso.model)
cv.group.lasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.group.lasso)
best_lambda <- cv.group.lasso$lambda.1se

# For de indkluderede varible har vi følgende estimater
beta_hat <- coef(group.lasso.model, s = best_lambda)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ] # 119 variable

# bootstrap ---------------------------------------------------------------

# Vha bootstrap metoden, vil vi quantificere variation af beta_hat.
# Vi resampler datasættet 1000 gange og udtrækker koefficienterne for hver realization
# Vi slicer bootstrap data til kun at indeholde de valgte variable

# Bootstrapping 
n_bootstrap <- 1000
unrate_n <- nrow(X.train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), simplify = FALSE)

unrate_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(gglasso(X.train[idx, ], y.train[idx], group = grp, intercept = FALSE, lambda = best_lambda)))
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
  ggtitle("Bootstrap Samples")

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
  ggtitle("Bootstrap Probability of 0")

grid.arrange(gg1, gg2, ncol = 2)
