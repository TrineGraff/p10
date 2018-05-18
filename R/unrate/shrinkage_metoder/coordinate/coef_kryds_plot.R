source("data_unrate.R")
source("package.R")
library(data.table)
set.seed(1)

# lasso
lasso_fit = glmnet(x_train, y_train, family = "gaussian", alpha = 1, intercept = FALSE,
                   standardize=FALSE)
lasso_cv = cv.glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1,
                     standardize=FALSE)
coef_lasso = as.vector(coef(lasso_fit, s = lasso_cv$lambda.1se)) %>% .[-1]
idx_lasso = which(coef_lasso != 0) 
coef_lasso[idx_lasso]     


## adaptive lasso med ols vægte
fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_cv = cv.glmnet(x_train, y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize = FALSE, 
                        penalty.factor = v)

adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v)

coef_adp_ols = as.vector(coef(adap_ols_fit, s = adap_ols_cv$lambda.min)) %>% .[-1]
idx_adp_ols = which(coef_adp_ols != 0) 
coef_adp_ols[idx_adp_ols]    


## adaptive lasso med lasso vægte
beta_lasso = as.vector(coef(lasso_fit, s = lasso_cv$lambda.1se)) %>% .[-1] #fjerner skæringen 
idx_beta = which(beta_lasso != 0)
v_l = 1/abs(beta_lasso[idx_beta]) 

adap_lasso_cv <- cv.glmnet(x_train[,idx_beta], y_train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                             penalty.factor = v_l)

adap_lasso_fit = glmnet(x_train[,idx_beta], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

coef_adp_lasso = coef(adap_lasso_fit, s = adap_lasso_cv$lambda.min)
idx_adp_lasso = which(coef_adp_lasso != 0) 
coef_adp_lasso[idx_adp_lasso,]  
coef_adp_lasso_vec = as.vector(coef(adap_lasso_fit, s = adap_lasso_cv$lambda.min)) %>% .[-1]
coef_adap_lasso = c(rep(0, 21), coef_adp_lasso_vec [4], coef_adp_lasso_vec [5], rep(0, 103))


# plots -------------------------------------------------------------------
## lasso, elnet, adaptiv lasso
coef = data.table("Lasso" = coef_lasso,
                  "Adap. lasso m. OLS vægte" = coef_adp_ols,
                  "Adap. lasso m. lasso vægte" = coef_adap_lasso)
coef[, feature := colnames(x_train)]

koef <- coef[feature == "DPCERA3M086SBEA" | feature == "IPDMAT" | feature == "HWIURATIO" 
             | feature == "CLF16OV"| feature == "CE16OV" | feature == "UEMPLT5" 
             | feature == "UEMP5TO14" | feature == "UEMP15OV" | feature == "PAYEMS" 
             | feature == "USCONS" |feature == "TB6MS" | feature == "GS5" 
             | feature == "EXUSUKx" | feature == "lag1" ]
               

to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
col_plot_1 = c("blue3", "blue3", "red3", "orange", "orange", "blue3", "chartreuse4", "blue3", 
               "blue3", "orange", "blue3", "blue3", "blue3", "blue3")
  
  

ggplot(to_plot, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot_1 ))





