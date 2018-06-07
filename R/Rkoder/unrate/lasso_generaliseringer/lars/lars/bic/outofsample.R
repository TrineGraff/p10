source("lasso_generaliseringer/lars/forecast.R")
source("lasso_generaliseringer/lars/lars/bic/insample.R")
source("mae_mse.R")

fc_lars = ForecastLARS(y,x, idx, s = lars_bic$f_hat, 
                         type = "lar", mode = "fraction" )

mae(y_test - fc_lars)
mse(y_test - fc_lars)



