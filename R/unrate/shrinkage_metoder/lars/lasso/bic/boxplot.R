source("shrinkage_metoder/lars/lasso/bic/insample.R")

n_bootstrap <- 1000
n <- nrow(x_train)
bootstrap_idx <- replicate(n_bootstrap, sample(n, size = n, replace = TRUE), 
                           simplify = FALSE)
beta_hat = coef(lasso_fit, s = lasso_bic$f_hat, mode = "fraction")


idx_hat = which(beta_hat != 0)
lasso_boot <- sapply(bootstrap_idx, function(idx) {
  fit = lars(x_train[idx,], y_train[idx], type = "lar", trace = TRUE,
             normalize = FALSE, intercept = FALSE)
  as.vector(coef(fit, s = lasso_bic$f_hat, mode = "fraction"))
})

lasso_boot_hat <- t(lasso_boot[idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(names(idx_hat))

colplot = c("red3", "chartreuse4", "blue3", "blue3", "blue3", "blue3",
            "blue3","blue3","blue3","blue3", "red3", "orange", "orange","orange",
            "cadetblue2", "blue3", "blue3")
lars1 <- lasso_boot_hat %>% 
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

lars2 <- lasso_boot_hat %>% 
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

grid.arrange(lars1, lars2, ncol = 2)
