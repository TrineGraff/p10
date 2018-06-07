source("lasso_generaliseringer/coordinate/group_lasso/bic/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

fc_gglasso = ForecastGL(y, x, idx = idx, group = grp, grp_bic$lambda)

mae(y_test - fc_gglasso )
mse(fc_gglasso  - y_test)

