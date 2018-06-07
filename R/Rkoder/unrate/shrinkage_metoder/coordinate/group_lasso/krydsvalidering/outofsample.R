source("shrinkage_metoder/coordinate/group_lasso/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")


fc_gglasso = gglasso_forecast(y, x, idx = idx, group = grp, gglasso_cv$lambda.1se)

mae(y_test - fc_gglasso )
mse(fc_gglasso  - y_test)

