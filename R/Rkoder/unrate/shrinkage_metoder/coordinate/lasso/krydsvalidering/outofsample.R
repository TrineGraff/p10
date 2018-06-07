source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

lasso_fc = glmnet_forecast(y, x, alpha = 1, idx = (idx), lasso_cv$lambda.1se)

mae(y_test - lasso_fc)
mse(lasso_fc - y_test)
