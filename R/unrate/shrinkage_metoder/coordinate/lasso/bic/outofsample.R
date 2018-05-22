source("shrinkage_metoder/coordinate/lasso/bic/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

lasso_fc = glmnet_forecast(y, x, alpha = 1, idx = (idx), fit_bic_lasso$lambda)

mae(y_test - lasso_fc)
mse(lasso_fc - y_test)

c = tidy(lasso_fc)
 write.csv(c, file = "fc_lasso_bic")
 