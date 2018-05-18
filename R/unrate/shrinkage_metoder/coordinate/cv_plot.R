source("data_unrate.R")
source("package.R")
set.seed(1)

# lasso -------------------------------------------------------------------
lasso_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", 
                     alpha = 1, standardize=FALSE)

# ridge -------------------------------------------------------------------
ridge_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", 
                     alpha = 0, standardize=FALSE)

# Group lasso -------------------------------------------------------------
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 
gglasso_cv <- cv.gglasso(x_train, y_train, group = grp, nfold = 10, intercept = FALSE, loss = "ls" )


# adaptive lasso m. ols ---------------------------------------------------
fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)
adap_ols_cv = cv.glmnet(x_train, y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v)


# adaptive lasso m. lasso -------------------------------------------------
lasso_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", 
                   alpha = 1, standardize=FALSE)
beta_hat = as.vector(coef(lasso_fit, s = lasso_cv$lambda.1se)) %>% .[-1]
idx_hat = which(beta_hat != 0) 
v_l = 1/abs(beta_hat[idx_hat]) #intercept er inkluderet
adap_lasso_cv = cv.glmnet(x_train[,idx_hat], y_train, intercept = FALSE, 
                          family = "gaussian", alpha = 1, standardize = FALSE, 
                          penalty.factor = v_l)



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

df_grp = data.frame(gglasso_cv$lambda, gglasso_cv$cvm, gglasso_cv$cvsd)

grp = ggplot(df_grp, aes(log(df_grp$gglasso_cv.lambda),df_grp$gglasso_cv.cvm )) + 
  geom_errorbar(aes(ymin = df_grp$gglasso_cv.cvm + df_grp$gglasso_cv.cvsd, 
                    ymax = df_grp$gglasso_cv.cvm - df_grp$gglasso_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(gglasso_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(gglasso_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Group lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


df_adap = data.frame(adap_ols_cv$lambda, adap_ols_cv$cvm, adap_ols_cv$cvsd)

  ad = ggplot(df_adap, aes(log(df_adap$adap_ols_cv.lambda),df_adap$adap_ols_cv.cvm )) + 
      geom_errorbar(aes(ymin = df_adap$adap_ols_cv.cvm + df_adap$adap_ols_cv.cvsd, 
                        ymax = df_adap$adap_ols_cv.cvm - df_adap$adap_ols_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(adap_ols_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(adap_ols_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Adap. lasso m. OLS vægte") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))

  
df_ad_l = data.frame(adap_lasso_cv$lambda, adap_lasso_cv$cvm, adap_lasso_cv$cvsd)
  
ad_l = ggplot(df_ad_l, aes(log(df_ad_l$adap_lasso_cv.lambda),df_ad_l$adap_lasso_cv.cvm)) + 
  geom_errorbar(aes(ymin = df_ad_l$adap_lasso_cv.cvm + df_ad_l$adap_lasso_cv.cvsd, 
                    ymax = df_ad_l$adap_lasso_cv.cvm - df_ad_l$adap_lasso_cv.cvsd, width = .1)) +
  geom_point(col = "red") +
  labs(x = expression(log(lambda)), y = "MSE", color = "") + 
  geom_vline(aes(xintercept= log(adap_lasso_cv$lambda.min), col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= log(adap_lasso_cv$lambda.1se), col = "brown"), linetype="dotted") +
  ggtitle("Adap. lasso m. lasso vægte") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


cv_plot = grid.arrange(l, r, grp, ad, ad_l, ncol = 2)


# Gem resultater ----------------------------------------------------------
library(broom)
c <- tidy(coef(lasso_cv, s="lambda.min"))
write.csv(c, file = "ridge_lambda") 
