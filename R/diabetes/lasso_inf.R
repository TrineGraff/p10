diabetes <- read.csv("diabetes.csv")

diabetes_design <- model.matrix(prog ~ . - 1, data = diabetes)
x <- scale(diabetes_design, center = TRUE, scale = TRUE)
y <- diabetes$prog - mean(diabetes$prog)

fit = glmnet(x, y, intercept = FALSE, standardize = FALSE, alpha = 1)
cv = cv.glmnet(x, y, intercept = FALSE, standardize = FALSE, alpha = 1)
lambda = coef(fit, s = cv$lambda.min)

n <- length(y)
lambda = cv$lambda.min * n
beta = coef(fit, s = cv$lambda.min)[-1]
idx_beta = which(beta!= 0)
fixed_lasso = fixedLassoInf(x, y, beta, lambda = lambda, 
                            intercept = FALSE, alpha = 0.5)


kryds = data.frame(fixed_lasso$ci, names(fixed_lasso$vars))
colnames(kryds) = c("lo_kryds", "up_kryds", "variabler")



##plotter intervallet for lasso og ols koefficienten. 
lm = lm(y ~ 0 + x[,idx_beta])
cf_lm = as.matrix(confint(lm, level = 0.9))
rownames(cf_lm) = NULL
name = colnames(x[,idx_beta])


model1Frame <- data.frame(Variable = name,
                          ci_low = kryds$lo_kryds,
                          ci_up = kryds$up_kryds,
                          modelName = "Lasso (CV) interval",
                          punkt = beta[idx_beta])

model2Frame <- data.frame(Variable = name,
                          ci_low = cf_lm[,1],
                          ci_up = cf_lm[,2],
                          modelName = "OLS interval", 
                          punkt = lm$coefficients)


allModelFrame_nu = data.frame(rbind(model1Frame, model2Frame))

ggplot(allModelFrame_nu, aes(colour = modelName)) + 
  coord_flip() +
  geom_linerange(aes(x = Variable, ymin = ci_low, ymax = ci_up),
                 lwd = 1, position = position_dodge(width = 1/2))+
  geom_point(aes(x = Variable, y = punkt),  position = position_dodge(width = 1/2)) +
  xlab("") + ylab("")+
  theme(legend.title=element_blank()) +
  geom_hline(aes(yintercept = 0) , linetype="dotted")

