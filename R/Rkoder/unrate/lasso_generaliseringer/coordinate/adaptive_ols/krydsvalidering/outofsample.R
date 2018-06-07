source("lasso_generaliseringer/coordinate/adaptive_ols/krydsvalidering/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

fc_adap_ols = ForecastAL(y, x, alpha = 1, idx = idx, adap_ols_cv$lambda.min, w = v_0)

mae(y_test - fc_adap_ols)
mse(fc_adap_ols - y_test)

