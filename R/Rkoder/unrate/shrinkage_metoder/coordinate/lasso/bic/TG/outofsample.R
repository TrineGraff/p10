source("shrinkage_metoder/coordinate/lasso/bic/TG/insample.R")
source("shrinkage_metoder/coordinate/lasso/TG_forecast.R")

fc = ForecastTG(y, x, idx = 552, lambda)

