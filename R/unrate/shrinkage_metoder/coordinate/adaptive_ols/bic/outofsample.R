source("shrinkage_metoder/coordinate/adaptive_ols/bic/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

fc_adap_ols = adap_forecast(y, x, alpha = 1, idx = (idx), adap_ols_bic$lambda, w = v_2)

mae(y_test - fc_adap_ols)
mse(fc_adap_ols - y_test)

c = tidy(fc_adap_ols)
write.csv(c, file = "fc_lasso_ols_bic.csv") 
