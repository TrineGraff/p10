source("data_unrate.R")
source("package.R")
source("shrinkage_metoder/coordinate/bic.R")
source("parm.R")

set.seed(1)

n_bootstrap = 1000
unrate_n <- nrow(x_train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), 
                           simplify = FALSE)

fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)
coef_lasso = coef(fit_lasso, s = fit_bic_lasso$lambda)
idx_lasso = which(coef_lasso != 0) 
 
lasso_boot<- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(x_train[idx,], y_train[idx], alpha = 1, lambda = fit_bic_lasso$lambda,
                        standardize = FALSE, intercept = FALSE)))
})

lasso_boot_hat <- t(lasso_boot[idx_lasso, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(coef_lasso)[idx_lasso])

col_plot = c("red3", "chartreuse4", "blue3", "blue3", "blue3", "blue3", "blue3", "blue3",
             "blue3", "blue3", "red3", "orange", "orange", "orange", "cadetblue2", "blue3", "blue3")
lasso1 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(coef_lasso)[idx_lasso ])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab("") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot))

lasso2 <- lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels =  rownames(coef_lasso)[idx_lasso ])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot))

grid.arrange(lasso1, lasso2, ncol = 2)

