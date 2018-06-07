source("shrinkage_metoder/coordinate/lasso/krydsvalidering/TG/insample.R")
source("shrinkage_metoder/coordinate/lasso/TG_forecast.R")
lambda 
fc = ForecastTG(y, x, idx, lambda)

c = tidy(fc)
write.csv(c, file = "lasso_TG_cv.csv") 
