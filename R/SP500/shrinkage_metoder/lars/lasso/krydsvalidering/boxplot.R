source("data_unrate.R")
source("shrinkage_metoder/lars/lasso/krydsvalidering/insample.R")

beta_hat = coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")


n_bootstrap = 1000
unrate_n <- nrow(x_train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), 
                           simplify = FALSE)
lasso_boot <- sapply(bootstrap_idx, function(idx) {
  fit = lars(x_train[idx,], y_train[idx], type = "lasso", trace = TRUE,
             normalize = FALSE, intercept = FALSE)
  as.vector(coef(fit, s = getmin$lambda.1se, mode = "fraction"))
})

lasso_boot_hat <- t(lasso_boot[idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(names(idx_hat))

colplot = c("chartreuse4", "blue3", "blue3", "blue3", 
            "blue3", "blue3", "blue3", "blue3",  "blue3", "blue3", 
            "orange", "orange", "blue3")
lasso1 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = names(beta_hat))) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab("") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = colplot))

lasso2 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels =  names(beta_hat))) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = colplot))

grid.arrange(lasso1, lasso2, ncol =2)
