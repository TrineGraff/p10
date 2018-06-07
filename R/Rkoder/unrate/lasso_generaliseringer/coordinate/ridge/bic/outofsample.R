source("lasso_generaliseringer/coordinate/ridge/bic/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

ridge_fc = Forecast(y, x, alpha = 0, idx = idx, fit_bic_ridge$lambda)

mae(y_test - ridge_fc)
mse(ridge_fc - y_test)


