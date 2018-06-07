source("shrinkage_metoder/coordinate/adaptive_lasso/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")


fc_adap_lasso = adap_forecast(y, x[,idx_beta], alpha = 1, idx = (idx), 
                            adap_lasso_cv$lambda.min, w = v_l)


mae(y_test - fc_adap_lasso)
mse(y_test - fc_adap_lasso)

