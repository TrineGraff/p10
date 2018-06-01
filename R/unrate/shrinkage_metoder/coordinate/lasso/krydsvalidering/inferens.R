source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

n <- length(y_train)
coef_kryds = coef(lasso_fit, s = lasso_cv$lambda.1se, exact=TRUE,x=x_train,y=y_train)[-1]
lambda_kryds = lasso_cv$lambda.1se * n
fixed_lasso_kryds = fixedLassoInf(x_train, y_train, coef_kryds, lambda = lambda_kryds, 
                                  intercept = FALSE, alpha = 0.1)

ci_kryds = data.frame(fixed_lasso_kryds$ci, fixed_lasso_kryds$vars)
colnames(ci_kryds) = c("lo_kryds", "up_kryds", "variabler")

#ols på disse estimater
lm = lm(y_train ~ 0+ x_train[,idx_hat - 1])
cf_lm = confint(lm, level = 0.9) 
rownames(cf_lm) = NULL
colnames(cf_lm) = c("lo_ols", "up_ols")
name = colnames(x_train[,idx_hat - 1])

names(lm$coefficients) = NULL


model1Frame <- data.frame(Variable = name,
                          ci_low = ci_kryds[,1],
                          ci_up = ci_kryds[,2],
                          modelName = "Lasso (CV) interval",
                          punkt = coef_1sd[idx_hat], 
                          par_punkt = fixed_lasso_kryds$coef0)

model2Frame <- data.frame(Variable = name,
                          ci_low = cf_lm[,1],
                          ci_up = cf_lm[,2],
                          modelName = "OLS interval", 
                          punkt = lm$coefficients, 
                          par_punkt = fixed_lasso_kryds$coef0)


allModelFrame_nu = data.frame(rbind(model1Frame, model2Frame))
col_plot = c("blue3", "blue3", "red3", "orange", "orange", "chartreuse4", "blue3","blue3", "blue3",
             "orange", "blue3","blue3","blue3","blue3")

kryds = ggplot(allModelFrame_nu, aes(colour = modelName)) + 
  coord_flip() +
  geom_linerange(aes(x = Variable, ymin = ci_low, ymax = ci_up),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") +
  theme(legend.position = "none") + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot)) +
  ggtitle("Lasso (CV)")
  

grid.arrange(kryds, bic, ncol = 2)
