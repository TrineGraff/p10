source("lasso_generaliseringer/lars/forecast.R")
source("lasso_generaliseringer/lars/lars/krydsvalidering/insample.R")
source("mae_mse.R")

fc_lars = ForecastLARS(y, x, idx, s = getmin_lars$lambda.1se, 
                        type = "lar", mode = "step" )
mae(y_test - fc_lars)
mse(y_test - fc_lars)


