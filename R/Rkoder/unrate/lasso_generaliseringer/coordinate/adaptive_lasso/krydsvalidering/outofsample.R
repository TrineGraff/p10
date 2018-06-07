source("lasso_generaliseringer/coordinate/adaptive_lasso/krydsvalidering/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

fc_adap_lasso = ForecastAL(y, x[,idx_lasso], alpha = 1, idx = idx, 
                            adap_lasso_cv$lambda.min, w = v_l)


mae(y_test - fc_adap_lasso)
mse(y_test - fc_adap_lasso)

