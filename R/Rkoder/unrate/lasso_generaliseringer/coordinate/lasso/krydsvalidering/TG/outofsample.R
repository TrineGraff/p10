source("lasso_generaliseringer/coordinate/lasso/krydsvalidering/TG/insample.R")
source("lasso_generaliseringer/coordinate/lasso/TG_forecast.R")
source("mae_mse.R")
fc = ForecastTG(y, x, idx, lambda)

mae(y_test - fc)
mse(fc - y_test)
