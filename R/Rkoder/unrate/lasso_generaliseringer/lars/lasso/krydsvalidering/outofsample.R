source("lasso_generaliseringer/lars/forecast.R")
source("lasso_generaliseringer/lars/lasso/krydsvalidering/insample.R")
source("mae_mse.R")

fc_lasso = ForecastLARS(y, x, idx, s = getmin$lambda.1se, 
                        type = "lasso", mode = "fraction")
mae(y_test - fc_lasso)
mse(y_test - fc_lasso)

