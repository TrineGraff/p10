source("shrinkage_metoder/coordinate/ridge/bic/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

ridge_fc = glmnet_forecast(y, x, alpha = 0, idx = (idx), fit_bic_ridge$lambda)

mae(y_test - ridge_fc)
mse(ridge_fc - y_test)


