source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/lasso/bic/insample.R")

n <- length(y_train)

# vil kun se på intervallet for variablerne, som er de mest valgte, dvs CLF16OV, CE16OV, IPDMAT, 
#UEMPLT5, UEM5TO14, UEM15OV, TB&MS, GS5, lag1. 
#22,23,14,25,26,27, 78, 80, 123

# krydsvalidering test ----------------------------------------------------

coef_kryds = coef(lasso_fit, s = lasso_cv$lambda.1se, exact=TRUE,x=x_train,y=y_train)[-1]
lambda_kryds = lasso_cv$lambda.1se * n
fixed_lasso_kryds = fixedLassoInf(x_train, y_train, coef_kryds, lambda = lambda_kryds, 
                            intercept = FALSE, alpha = 0.1)

round(data.frame(fixed_lasso_kryds$vlo, fixed_lasso_kryds$vup, fixed_lasso_kryds$coef0), digits = 3)

ci_kryds = rbind(fixed_lasso_kryds$ci[4,], c(fixed_lasso_kryds$ci[5,]), fixed_lasso_kryds$ci[2,], 
        fixed_lasso_kryds$ci[6,], fixed_lasso_kryds$ci[7,], fixed_lasso_kryds$ci[8,],  
        fixed_lasso_kryds$ci[11,],  fixed_lasso_kryds$ci[12,],  fixed_lasso_kryds$ci[14,])
colnames(ci_kryds) = c("lo_kryds", "up_kryds")

# BIC test ----------------------------------------------------

coef_bic = coef(lasso_fit, s = fit_bic_lasso$lambda, exact=TRUE,x=x_train,y=y_train)[-1]
lambda_bic = fit_bic_lasso$lambda * n
fixed_lasso_bic = fixedLassoInf(x_train, y_train, coef_bic, lambda = lambda_bic, 
                            intercept = FALSE, alpha = 0.1) 
round(data.frame(fixed_lasso_bic$vlo, fixed_lasso_bic$vup, fixed_lasso_bic$coef0), digits = 3)

ci_bic = rbind(fixed_lasso_bic$ci[3,], fixed_lasso_bic$ci[4,], fixed_lasso_bic$ci[2,], fixed_lasso_bic$ci[5,],
               fixed_lasso_bic$ci[6,], fixed_lasso_bic$ci[7,], fixed_lasso_bic$ci[12,], fixed_lasso_bic$ci[13,], fixed_lasso_bic$ci[16,])
colnames(ci_bic) = c("lo_bic", "up_bic")

# OLS test ----------------------------------------------------

#vi anvender OLS på disse estimater
name  = c(colnames(x_train)[22],colnames(x_train)[23], colnames(x_train)[14], colnames(x_train)[25], 
               colnames(x_train)[26], colnames(x_train)[27], colnames(x_train)[78], colnames(x_train)[80], colnames(x_train)[123]) 

x_ols = data.frame(x_22 = x_train[,22], x_23 = x_train[,23], x_14 = x_train[,14], x_25 =  x_train[,25], 
                 x_26 =  x_train[,26], x_27 = x_train[,27], x_78 = x_train[,78], x_80 = x_train[,80], x_123 = x_train[,123])

lm = lm(y_train ~ 0+ as.matrix(x_ols))
cf_lm = confint(lm, level = 0.9) 
rownames(cf_lm) = NULL
colnames(cf_lm) = c("lo_ols", "up_ols")


model1Frame <- data.frame(Variable = name,
                          ci_low = ci_kryds[,1],
                          ci_up = ci_kryds[,2],
                          modelName = "Lasso (CV) interval")
model2Frame <- data.frame(Variable = name,
                          ci_low = ci_bic[,1],
                          ci_up = ci_bic[,2],
                            modelName = "Lasso (BIC) interval")
model3Frame <- data.frame(Variable = name,
                          ci_low = cf_lm[,1],
                          ci_up = cf_lm[,2],
                          modelName = "OLS interval")
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame))  
col_plot = c("blue3", "blue3", "orange", "chartreuse4", "blue3", "orange", "blue3", "blue3", "blue3")



ggplot(allModelFrame, aes(colour = modelName)) + 
  coord_flip() +
  geom_linerange(aes(x = Variable, ymin = ci_low,ymax = ci_up),
                            lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") +
  theme(legend.title=element_blank()) + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot))
  

# z-score -----------------------------------------------------------------
zscore = fixed_lasso_kryds$coef0 /fixed_lasso_kryds$sd
?fixed_lasso_kryds
vmat = fixed_lasso_kryds$vmat
sd = fixed_lasso_kryds$sigma*sqrt(rowSums(vmat^2)) 
zscore = fixed_lasso_kryds$coef0 / sd
?fixedLassoInf
