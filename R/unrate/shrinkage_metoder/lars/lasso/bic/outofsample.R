source("shrinkage_metoder/lars/forecast.R")
source("shrinkage_metoder/lars/lasso/bic/insample.R")
source("mae_mse.R")

fc_lasso = lars_forecast(y,x, idx, s = lasso_bic$f_hat, 
                         type = "lasso", mode = "fraction" )
mae(y_test - fc_lasso)
mse(y_test - fc_lasso)

c = tidy(fc_lasso)
write.csv(c, file = "fc_lars_lasso_bic.csv") 
