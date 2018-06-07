source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

coef_bic = coef(fit_lasso, s = fit_bic_lasso$lambda)[-1]

n <- length(y_train)
lambda_bic = fit_bic_lasso$lambda * n
fixed_lasso_bic = fixedLassoInf(x_train, y_train, coef_bic, lambda = lambda_bic, 
                                  intercept = FALSE, alpha = 0.1)

ci_bic = data.frame(fixed_lasso_bic$ci, fixed_lasso_bic$vars)
colnames(ci_bic) = c("lo_kryds", "up_kryds", "variabler")


#ols på disse estimater
lm_bic = lm(y_train ~ 0+ x_train[,idx_bic - 1])
cf_lm_bic = confint(lm, level = 0.9) 
rownames(cf_lm_bic) = NULL
colnames(cf_lm_bic) = c("lo_ols", "up_ols")
name = colnames(x_train[,idx_bic - 1])

names(lm_bic$coefficients) = NULL



model1Frame_bic <- data.frame(Variable = name,
                          ci_low = ci_bic[,1],
                          ci_up = ci_bic[,2],
                          modelName = "Lasso (BIC) interval",
                          punkt_bic = fixed_lasso_bic$coef0)

model2Frame_bic <- data.frame(Variable = name,
                          ci_low = cf_lm_bic[,1],
                          ci_up = cf_lm_bic[,2],
                          modelName = "OLS interval", 
                          punkt_bic = lm_bic$coefficients)

allModelFrame_bic = data.frame(rbind(model1Frame_bic, model2Frame_bic))
col_plot_bic = c("red3", "blue3","blue3", "blue3", "cadetblue2", "red3", "orange", "orange",
                 "chartreuse4", "blue3", "blue3", "orange", "blue3", "blue3", "blue3", "blue3", "blue3")

bic = ggplot(allModelFrame_bic, aes(colour = modelName)) + 
  coord_flip() +
  geom_linerange(aes(x = Variable, ymin = ci_low, ymax = ci_up),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_point(aes(x = Variable, y = punkt_bic), lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") + ylab(" ") +
  theme(legend.position = "none") + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot_bic)) +
  ggtitle(expression(Lasso[TG](BIC)))

