source("shrinkage_metoder/coordinate/ridge/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

ridge_fc = Forecast(y, x, alpha = 0, idx = (idx), ridge_cv$lambda.min)

mae(y_test - ridge_fc)
mse(ridge_fc - y_test)

