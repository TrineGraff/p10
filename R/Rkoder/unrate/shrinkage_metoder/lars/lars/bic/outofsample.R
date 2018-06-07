source("shrinkage_metoder/lars/forecast.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")
source("mae_mse.R")

fc_lars = lars_forecast(y,x, idx, s = lars_bic$s, 
                         type = "lar", mode = "step" )

mae(y_test - fc_lars)
mse(y_test - fc_lars)

c = tidy(fc_lars)
write.csv(c, file = "fc_lars_bic.csv") 

