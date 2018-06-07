source("lasso_generaliseringer/coordinate/group_lasso/krydsvalidering/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

fc_gglasso = ForecastGL(y, x, idx = idx, group = grp, gglasso_cv$lambda.1se)

mae(y_test - fc_gglasso )
mse(fc_gglasso - y_test)

