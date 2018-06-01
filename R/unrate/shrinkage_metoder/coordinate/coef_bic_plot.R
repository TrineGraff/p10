source("data_unrate.R")
source("package.R")
source("shrinkage_metoder/coordinate/bic.R")
source("parm.R")

##lasso
fit_lasso = glmnet(x_train, y_train, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y_train, x_train, fit_lasso)
coef_lasso = as.vector(coef(fit_lasso , s = fit_bic_lasso$lambda)) %>% .[-1]
idx_lasso = which(coef_lasso != 0) 


##adaptive lasso med ols vægte
fit_ols = lm(y_train ~ 0 + x_train)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)^2


adap_ols_fit = glmnet(x_train, y_train, intercept = FALSE, family = "gaussian", alpha = 1, 
                      standardize=FALSE, penalty.factor = v)
adap_ols_bic = lassoBIC(y_train, x_train, adap_ols_fit)
coef_adap_ols = as.vector(coef(adap_ols_fit , s = adap_ols_bic$lambda)) %>% .[-1]
idx_adap_ols= which(coef_adap_ols != 0) 

##adaptive lasso med lasso vægte

v_l = 1/abs(coef_lasso[idx_lasso])^0.5 
adap_lasso_fit = glmnet(x_train[,idx_lasso], y_train, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso_bic = lassoBIC(y_train,x_train[,idx_lasso], adap_lasso_fit)

coef_adap_lasso_vec = as.vector(coef(adap_lasso_fit , s = adap_lasso_bic$lambda)) %>% .[-1]
idx_adap_ols = which(coef_adap_ols != 0) 

x_ny = x_train[,idx_lasso]
colnames(x_train)[123]
coef_adap_lasso = c(rep(0, 21), coef_adap_lasso_vec[3], coef_adap_lasso_vec[4], rep(0, 99), coef_adap_lasso_vec[16], rep(0, 3))


# plots -------------------------------------------------------------------
## lasso, elnet, adaptiv lasso
coef = data.table("Lasso" = coef_lasso,
                  "Adap. lasso m. OLS vægte" = coef_adap_ols,
                  "Adap. lasso m. lasso vægte" = coef_adap_lasso)
coef[, feature := colnames(x_train)]
idx = which(test != 0)

koef <- coef[feature == "DPCERA3M086SBEA" | feature == "IPDMAT" | feature == "CLF16OV"
             | feature == "CE16OV" | feature == "UEMPLT5"
             | feature == "UEMP5TO14" | feature == "UEMP15OV" | feature == "CLAIMSx"
             | feature == "USCONS" | feature =="USTRADE" | feature =="AMDMNOx" 
             | feature == "TB6MS" | feature == "GS5" 
             | feature == "EXUSUKx" |feature == "CPIMEDSL" | feature == "lag1" | feature =="lag4" ]

to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")

col_plot = c("red3", "blue3", "blue3", "blue3", "cadetblue2", "red3", "orange", 
             "orange", "chartreuse4", "blue3", "blue3", "orange","blue3", "blue3", "blue3", "blue3", "blue3" )


ggplot(to_plot, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot))

