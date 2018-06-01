source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

n <- length(y_train)
lambda <- fit_bic_lasso$lambda * n
beta = coef(fit_lasso, s = lambda/n, exact = TRUE, x = x_train, y = y_train)[-1]
out_glm <- fixedLassoInf(x_train, y_train, beta, lambda, intercept = FALSE, alpha = 0.1)

idx_bic = which(beta !=0 )
colnames(x_train[,idx_bic])

ci_bic = data.frame(out_glm $ci, out_glm$vars)
colnames(ci_bic) = c("lo_kryds", "up_kryds", "variabler")


#ols på disse estimater
lm_bic = lm(y_train ~ 0+ x_train[,idx_bic])
cf_bic = confint(lm_bic, level = 0.9) 
rownames(cf_bic) = NULL
colnames(cf_bic) = c("lo_ols", "up_ols")
name = colnames(x_train[,idx_bic])

names(lm$coefficients) = NULL


model1Frame <- data.frame(Variable = name,
                          ci_low = ci_bic[,1],
                          ci_up = ci_bic[,2],
                          modelName = "Lasso (BIC) interval",
                          punkt = beta[idx_bic])

model2Frame <- data.frame(Variable = name,
                          ci_low = cf_bic[,1],
                          ci_up = cf_bic[,2],
                          modelName = "OLS interval", 
                          punkt = lm$coefficients)


allModelFrame_nu = data.frame(rbind(model1Frame, model2Frame))
col_plot = c("red3", "blue3", "blue3", "blue", "cadetblue2", "red3", "orange",
             "orange", "chartreuse4",  "blue3", "blue3", "orange", "blue3","blue3","blue3","blue3","blue3")
             
    

bic = ggplot(allModelFrame_nu, aes(colour = modelName)) + 
  coord_flip() +
  geom_linerange(aes(x = Variable, ymin = ci_low, ymax = ci_up),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") + ylab("")+
  theme(legend.position="none") + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot)) +
  ggtitle("Lasso (BIC)") +  guides(fill = FALSE) 


p1 = ggplotGrob(bic)
p2 = ggplotGrob(kryds)

grid.draw(cbind(p2, p1, size = "last"))

