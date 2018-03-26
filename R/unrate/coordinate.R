source("/Users/trinegraff/Desktop/Projekt/R/unrate/script/script.R")

parm = function(x) {
  (sum(x != 0))
}

# lasso -------------------------------------------------------------------

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(lasso_cv$lambda.min, lasso_cv$lambda.1se),
  error = with(lasso_cv, c(cvm[which(lambda == lasso_cv$lambda.min)], cvm[which(lambda == lasso_cv$lambda.1se)])),  
  p = apply(coef(lasso_fit, s = c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 2, parm) 
) 

b_hat = coef(lasso_fit, s = lasso_cv$lambda.1se)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


# ridge -------------------------------------------------------------------

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(ridge_cv$lambda.min, ridge_cv$lambda.1se),
  error = with(ridge_cv, c(cvm[which(lambda == ridge_cv$lambda.min)], cvm[which(lambda == ridge_cv$lambda.1se)])),  
  p = apply(coef(ridge_fit, s = c(ridge_cv$lambda.min, ridge_cv$lambda.1se)), 2, parm) 
) 

# Elastic Net -------------------------------------------------------------------

cv1_min  = c(lambda = fit0.1$lambda.min, cvm = fit0.1$cvm[fit0.1$lambda == fit0.1$lambda.min], alpha = 0.1)
cv2_min = c(lambda = fit0.188888888888889$lambda.min, 
  cvm_min = fit0.188888888888889$cvm[fit0.188888888888889$lambda == fit0.188888888888889$lambda.min], alpha = 0.188888888888889)
cv3_min = c(lambda = fit0.277777777777778$lambda.min, 
  cvm_min = fit0.277777777777778$cvm[fit0.277777777777778$lambda == fit0.277777777777778$lambda.min], alpha = 0.277777777777778 )
cv4_min = c(lambda = fit0.366666666666667$lambda.min, 
  cvm_min = fit0.366666666666667$cvm[fit0.366666666666667$lambda == fit0.366666666666667$lambda.min], alpha = 0.366666666666667)
cv5_min = c(lambda = fit0.455555555555556$lambda.min, 
  cvm_min = fit0.455555555555556$cvm[fit0.455555555555556$lambda == fit0.455555555555556$lambda.min], alpha =  0.455555555555556)
cv6_min = c(lambda = fit0.544444444444445$lambda.min, 
  cvm_min = fit0.544444444444445$cvm[fit0.544444444444445$lambda == fit0.544444444444445$lambda.min], alpha = 0.544444444444445)
cv7_min = c(lambda = fit0.633333333333333$lambda.min, 
  cvm_min = fit0.633333333333333$cvm[fit0.633333333333333$lambda == fit0.633333333333333$lambda.min], alpha = 0.633333333333333)
cv8_min = c(lambda = fit0.722222222222222$lambda.min, 
  cvm_min = fit0.722222222222222$cvm[fit0.722222222222222$lambda == fit0.722222222222222$lambda.min], alpha =0.722222222222222)
cv9_min = c(lambda = fit0.811111111111111$lambda.min,
  cvm_min = fit0.811111111111111$cvm[fit0.811111111111111$lambda == fit0.811111111111111$lambda.min], alpha = 0.811111111111111)
cv10_min = c(lambda = fit0.9$lambda.min, cvm_min = fit0.9$cvm[fit0.9$lambda == fit0.9$lambda.min], alpha = 0.9)

cv1_sd  = c(lambda.sd = fit0.1$lambda.1se, cvm_sd = fit0.1$cvm[fit0.1$lambda == fit0.1$lambda.1se], alpha = 0.1)
cv2_sd  = c(lambda.sd = fit0.188888888888889$lambda.1se, 
            cvm_sd = fit0.188888888888889$cvm[fit0.188888888888889$lambda == fit0.188888888888889$lambda.1se], alpha =0.188888888888889)
cv3_sd = c(lambda.sd = fit0.277777777777778$lambda.1se, 
           cvm_sd = fit0.277777777777778$cvm[fit0.277777777777778$lambda == fit0.277777777777778$lambda.1se], alpha = 0.277777777777778)
cv4_sd = c(lambda.sd = fit0.366666666666667$lambda.1se, 
           cvm_sd = fit0.366666666666667$cvm[fit0.366666666666667$lambda == fit0.366666666666667$lambda.1se], alpha = 0.366666666666667)
cv5_sd  = c(lambda.sd = fit0.455555555555556$lambda.1se, 
            cvm_sd = fit0.455555555555556$cvm[fit0.455555555555556$lambda == fit0.455555555555556$lambda.1se], alpha =0.455555555555556)
cv6_sd  = c(lambda.sd = fit0.544444444444445$lambda.1se, 
            cvm_sd = fit0.544444444444445$cvm[fit0.544444444444445$lambda == fit0.544444444444445$lambda.1se], alpha = 0.544444444444445)
cv7_sd  = c(lambda.sd = fit0.633333333333333$lambda.1se, 
            cvm_sd = fit0.633333333333333$cvm[fit0.633333333333333$lambda == fit0.633333333333333$lambda.1se], alpha = 0.633333333333333)
cv8_sd  = c(lambda.sd = fit0.722222222222222$lambda.1se, 
            cvm_sd = fit0.722222222222222$cvm[fit0.722222222222222$lambda == fit0.722222222222222$lambda.1se], alpha = 0.722222222222222)
cv9_sd  = c(lambda.sd = fit0.811111111111111$lambda.1se, 
            cvm_sd = fit0.811111111111111$cvm[fit0.811111111111111$lambda == fit0.811111111111111$lambda.1se], alpha = 0.811111111111111)
cv10_sd  = c(lambda.sd = fit0.9$lambda.1se, cvm_sd = fit0.9$cvm[fit0.9$lambda == fit0.9$lambda.1se], alpha = 0.9)


cv = data.frame(cv1_min, cv2_min, cv3_min, cv4_min, cv5_min, cv6_min, cv7_min, cv8_min, cv9_min, cv10_min, 
                cv1_sd, cv2_sd, cv3_sd, cv4_sd, cv5_sd, cv6_sd, cv7_sd, cv8_sd, cv9_sd, cv10_sd)

cv_min = data.frame(cv1_min, cv2_min, cv3_min, cv4_min, cv5_min, cv6_min, cv7_min, cv8_min, cv9_min, cv10_min)
cv_1sd = data.frame(cv1_sd, cv2_sd, cv3_sd, cv4_sd, cv5_sd, cv6_sd, cv7_sd, cv8_sd, cv9_sd, cv10_sd)
which.min(cv_1sd[2,] )

fit_el = glmnet(x, y, family = "gaussian", alpha = 0.9, intercept = FALSE )

data.frame(
  lambda = c("min", "1sd"), 
  lambda_val = c(fit0.9$lambda.min, fit0.9$lambda.1se),
  error = with(fit0.9, c(cvm[which(lambda == fit0.9$lambda.min)], cvm[which(lambda == fit0.9$lambda.1se)])),  
  p = apply(coef(fit_el, s = c(fit0.9$lambda.min, fit0.9$lambda.1se)), 2, parm)
) 
 
b_hat_el = coef(fit_el, s = fit0.9$lambda.1se)
idx_hat_el = which(b_hat_el != 0) 
b_hat_el[idx_hat_el, ]

# Group Lasso -------------------------------------------------------------------

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(gglasso_cv$lambda.min, gglasso_cv$lambda.1se),
  error = with(gglasso_cv, c(cvm[which(lambda == gglasso_cv$lambda.min)], cvm[which(lambda == gglasso_cv$lambda.1se)])),  
  p = apply(coef(gglasso_fit , s = c(gglasso_cv$lambda.min, gglasso_cv$lambda.1se)), 2, parm) 
) 


# Adaptive lasso med OLS----------------------------------------------------------

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(adap_ols$lambda.min, adap_ols$lambda.1se),
  error = with(adap_ols, c(cvm[which(lambda == adap_ols$lambda.min)], cvm[which(lambda == adap_ols$lambda.1se)])),  
  p = apply(coef(adap_ols_fit, s = c(adap_ols$lambda.min, adap_ols$lambda.1se)), 2, parm) 
) 

coef(adap_ols_fit, s = adap_ols$lambda.min)
# Adaptive lasso med Lasso vægte ------------------------------------------

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(adap_lasso$lambda.min, adap_lasso$lambda.1se),
  error = with(adap_lasso, c(cvm[which(lambda == adap_lasso$lambda.min)], cvm[which(lambda == adap_lasso$lambda.1se)])),  
  p = apply(coef(adap_lasso_fit, s = c(adap_lasso$lambda.min, adap_lasso$lambda.1se)), 2, parm) 
) 

coef(adap_lasso_fit, s = adap_lasso$lambda.min)

# plot --------------------------------------------------------------------
#getAnywhere(plot.cv.glmnet) #se plot kode

df_l = data.frame(lasso_cv$lambda, lasso_cv$cvm, lasso_cv$cvsd)

l = ggplot(df_l, aes(log(df_l$lasso_cv.lambda),df_l$lasso_cv.cvm )) + 
  geom_errorbar(aes(ymin = df_l$lasso_cv.cvm + df_l$lasso_cv.cvsd, 
                    ymax = df_l$lasso_cv.cvm - df_l$lasso_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(lasso_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(lasso_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))

df_r = data.frame(ridge_cv$lambda, ridge_cv$cvm, ridge_cv$cvsd)

r = ggplot(df_r, aes(log(df_r$ridge_cv.lambda),df_r$ridge_cv.cvm )) + 
  geom_errorbar(aes(ymin = df_r$ridge_cv.cvm + df_r$ridge_cv.cvsd, ymax = df_r$ridge_cv.cvm - df_r$ridge_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(ridge_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(ridge_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Ridge") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


df_el = data.frame(fit0.9$lambda, fit0.9$cvm, fit0.9$cvsd)
el = ggplot(df_el, aes(log(df_el$fit0.9.lambda),df_el$fit0.9.cvm )) + 
  geom_errorbar(aes(ymin = df_el$fit0.9.cvm + df_el$fit0.9.cvsd, ymax = df_el$fit0.9.cvm - df_el$fit0.9.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(fit0.9$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(fit0.9$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Elastic Net") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


df_grp = data.frame(gglasso_cv$lambda, gglasso_cv$cvm, gglasso_cv$cvsd)

grp = ggplot(df_grp, aes(log(df_grp$gglasso_cv.lambda),df_grp$gglasso_cv.cvm )) + 
  geom_errorbar(aes(ymin = df_grp$gglasso_cv.cvm + df_grp$gglasso_cv.cvsd, 
                    ymax = df_grp$gglasso_cv.cvm - df_grp$gglasso_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(gglasso_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(gglasso_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Group Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


df_adap = data.frame(adap_ols$lambda, adap_ols$cvm, adap_ols$cvsd)

  ad = ggplot(df_adap, aes(log(df_adap$adap_ols.lambda),df_adap$adap_ols.cvm )) + 
      geom_errorbar(aes(ymin = df_adap$adap_ols.cvm + df_adap$adap_ols.cvsd, 
                        ymax = df_adap$adap_ols.cvm - df_adap$adap_ols.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(adap_ols$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(adap_ols$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Group Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))

  
df_ad_l = data.frame(adap_lasso$lambda, adap_lasso$cvm, adap_lasso$cvsd)
  
ad_l = ggplot(df_ad_l, aes(log(df_ad_l$adap_lasso.lambda),df_ad_l$adap_lasso.cvm)) + 
  geom_errorbar(aes(ymin = df_ad_l$adap_lasso.cvm + df_ad_l$adap_lasso.cvsd, 
                    ymax = df_ad_l$adap_lasso.cvm - df_ad_l$adap_lasso.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(adap_lasso$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(adap_lasso$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Group Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


cv_plot = grid.arrange(l, r, el, grp, ad, ad_l)


# Gem resultater ----------------------------------------------------------
install.packages("broom")
library(broom)
c <- tidy(coef(lasso_cv, s="lambda.min"))
write.csv(c, file = "results") 
