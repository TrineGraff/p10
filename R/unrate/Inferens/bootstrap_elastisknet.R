source("../scripts/setup_data.R")
set.seed(1)

library(glmnet)
library(ggplot2)
library(gridExtra)

# hvoraf vi ser at y.hat6 giver mindst mse, dvs alpha = 0.6

EN.model <- glmnet(X.train, y.train, alpha=6/10, standardize = FALSE, intercept = FALSE)
cv.EN <- cv.glmnet(X.train, y.train, alpha=6/10, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
best_lambda <- cv.EN$lambda.1se

# For de indkluderede varible har vi følgende estimater
beta_hat <- coef(EN.model, s = best_lambda)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ] # 28 variable

# bootstrap ---------------------------------------------------------------

# Vha bootstrap metoden, vil vi quantificere variation af beta_hat.
# Vi resampler datasættet 1000 gange og udtrækker koefficienterne for hver realization
# Vi slicer bootstrap data til kun at indeholde de valgte variable

# Bootstrapping 
n_bootstrap <- 1000
unrate_n <- nrow(X.train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), simplify = FALSE)

unrate_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X.train[idx, ], y.train[idx], alpha = 6/10, lambda = best_lambda,
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


