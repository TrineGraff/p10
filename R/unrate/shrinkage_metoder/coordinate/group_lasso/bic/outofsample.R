source("shrinkage_metoder/coordinate/group_lasso/bic/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

fc_gglasso = gglasso_forecast(y, x, idx = idx, group = grp, grp_bic$lambda)

mae(y_test - fc_gglasso )
mse(fc_gglasso  - y_test)

c = tidy(fc_gglasso)
write.csv(c, file = "fc_gglasso_bic") 
