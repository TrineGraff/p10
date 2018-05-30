source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")

n <- length(y_train)
lambda = lasso_cv$lambda.1se * n
beta = coef(lasso_fit, s = lasso_cv$lambda.1se)[-1]
idx_beta = which(beta!= 0)
fixed_lasso = fixedLassoInf(x_train, y_train, beta, lambda = lambda, 
              intercept = FALSE, alpha = 0.5)


##plotter intervallet for lasso og ols koefficienten. 
lm = lm(y_train~ 0 +x_train[,idx_beta])
cf_lm = as.matrix(confint(lm, level = 0.9))
df = data.frame(fixed_lasso$ci, fixed_lasso$coef0, var = colnames(x_train[,idx_hat -1]), cf_lm)

ggplot(df, aes(df$var, df$fixed_lasso.coef0 )) + 
  geom_errorbar(aes(ymin = df$X1, 
                    ymax = df$X2, width = .1), col = "red") +
  geom_errorbar(aes(ymin = df$X5.., 
                    ymax = df$X95.., width = .1), col = "black") +
  labs(x = "Variabler", y = "Koefficient") 

