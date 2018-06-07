source("shrinkage_metoder/coordinate/lasso/krydsvalidering/TG/insample.R")
source("shrinkage_metoder/coordinate/lasso/TG_forecast.R")

fc = ForecastTG(y, x, idx, lambda)

