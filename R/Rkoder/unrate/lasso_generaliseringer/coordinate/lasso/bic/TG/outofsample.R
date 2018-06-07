source("lasso_generaliseringer/coordinate/lasso/bic/TG/insample.R")
source("lasso_generaliseringer/coordinate/lasso/TG_forecast.R")
source("mae_mse.R")

lasso_fc = ForecastTG(y, x, idx = 552, lambda)

mae(y_test - lasso_fc)
mse(lasso_fc - y_test)
