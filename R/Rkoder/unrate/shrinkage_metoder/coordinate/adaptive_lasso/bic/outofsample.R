source("shrinkage_metoder/coordinate/adaptive_lasso/bic/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")

adap_lasso_fc = adap_forecast(y, x[,idx_beta], alpha = 1, 
                                idx = (idx), lambda = adap_lasso_bic$lambda, w = v_l)

mae(y_test - adap_lasso_fc)
mse(adap_lasso_fc - y_test)

