source("shrinkage_metoder/lars/forecast.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")
source("mae_mse.R")

fc_lars = lars_forecast(y,x, idx, s = getmin_lars$lambda.1se , type = "lar", mode = "step" )
mae(y_test - fc_lars)
mse(y_test - fc_lars)

