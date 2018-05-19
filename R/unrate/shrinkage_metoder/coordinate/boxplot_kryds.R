source("data_unrate.R")
source("package.R")
set.seed(1)

n_bootstrap = 1000
unrate_n <- nrow(x_train)
bootstrap_idx <- replicate(n_bootstrap, sample(unrate_n, size = unrate_n, replace = TRUE), 
                           simplify = FALSE)


# lasso -------------------------------------------------------------------

lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE,
                   standardize=FALSE)
lasso_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1,
                     standardize=FALSE)
coef_lasso = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_lasso = which(coef_lasso != 0) 


lasso_boot<- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(x_train[idx,], y_train[idx], alpha = 1, lambda = lasso_cv$lambda.1se ,
                        standardize = FALSE, intercept = FALSE)))
})


lasso_boot_hat <- t(lasso_boot[idx_lasso, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(coef_lasso )[idx_lasso])

col_plot = c("red3", "chartreuse4", "blue3", "blue3", "blue3", "blue3",
             "blue3","blue3","blue3","blue3", "orange", "orange", "orange", "blue3")

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
  theme(plot.title = element_text(size = 10)) + 
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



# adaptive lasso med ols vægte --------------------------------------------
fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_cv = cv.glmnet(x_train, y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v)

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v)

coef_adp_ols = (coef(adap_ols_fit, s = adap_ols_cv$lambda.min))
idx_adp_ols = which(coef_adp_ols != 0) 


## bootstrap
adap_ols_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(x_train[idx,], y_train[idx], alpha = 1, lambda = adap_ols_cv$lambda.min,
                        penalty.factor = v, standardize = FALSE, intercept = FALSE)))
})

adap_ols_boot_hat <- t(adap_ols_boot[idx_adp_ols, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(coef_adp_ols )[idx_adp_ols])

adap_ols_plot = adap_ols_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(coef_adp_ols )[idx_adp_ols])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = "blue3"))

adap_ols_plot2 = adap_ols_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(coef_adp_ols )[idx_adp_ols])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = "blue3"))


# adaptive lasso med lasso vægte ------------------------------------------

beta_lasso = as.vector(coef(lasso_fit, s = lasso_cv$lambda.1se)) %>% .[-1] #fjerner skæringen 
idx_beta = which(beta_lasso != 0)
v_l = 1/abs(beta_lasso[idx_beta]) 

adap_lasso_cv <- cv.glmnet(x_train[,idx_beta], y_train, alpha = 1, standardize = FALSE, 
                           intercept = FALSE, 
                           penalty.factor = v_l)


adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

coef_adp_lasso = coef(adap_lasso_fit, s = adap_lasso_cv$lambda.min)
idx_adp_lasso = which(coef_adp_lasso != 0) 
coef_adp_lasso[idx_adp_lasso,] 

x_al = x_train[,idx_beta]

adap_lasso_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(x_al[idx,], y_train[idx], alpha = 1, lambda = adap_lasso_cv$lambda.min,
                        penalty.factor = v_l, standardize = FALSE, intercept = FALSE)))
})



adap_lasso_boot_hat <- t(adap_lasso_boot[idx_adp_lasso, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(coef_adp_lasso)[idx_adp_lasso])

adap_lasso_plot = adap_lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(coef_adp_lasso)[idx_adp_lasso])) %>% 
  group_by(Variable) %>% 
  ggplot(aes(Variable, Estimate)) +
  geom_boxplot(outlier.size = 0.1) + 
  coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Boxplot") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = "blue3"))

adap_lasso_plot2 = adap_lasso_boot_hat %>% 
  gather("Variable", "Estimate") %>% 
  mutate(Variable = factor(Variable, levels = rownames(coef_adp_lasso)[idx_adp_lasso])) %>% 
  group_by(Variable) %>% 
  summarise(Prob = sum(Estimate == 0) / n()) %>% 
  ggplot(aes(Variable, Prob)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sandsynlighed for 0") +
  theme(plot.title = element_text(size = 10))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = "blue3"))


alasso.OLS.plot <- grid.arrange(adap_ols_plot, adap_ols_plot2 , ncol = 2, top = "Adaptive lasso med OLS vægte")
alasso.lasso.plot <- grid.arrange(adap_lasso_plot, adap_lasso_plot2, ncol = 2, top = "Adaptive lasso med lasso vægte")
grid.arrange(alasso.OLS.plot, alasso.lasso.plot, ncol = 1)




