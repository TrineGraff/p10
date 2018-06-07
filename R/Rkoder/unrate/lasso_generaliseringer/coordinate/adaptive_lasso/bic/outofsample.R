source("lasso_generaliseringer/coordinate/adaptive_lasso/bic/insample.R")
source("lasso_generaliseringer/coordinate/forecasts.R")
source("mae_mse.R")

adap_lasso_fc = ForecastAL(y, x[,idx_lasso], alpha = 1, 
                                idx = idx, lambda = adap_lasso_bic$lambda, w = v_l)

mae(y_test - adap_lasso_fc)
mse(adap_lasso_fc - y_test)

