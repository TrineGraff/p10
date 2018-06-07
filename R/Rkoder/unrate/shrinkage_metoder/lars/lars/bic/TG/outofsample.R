source("shrinkage_metoder/lars/lars/forecastLarsTG.R")
source("shrinkage_metoder/lars/lars/bic/TG/insample.R")

fc = ForecastLarsTG(y, x, idx = 552, (lars_bic$p +1))

c = tidy(fc)
write.csv(c, file = "lars_TG_bic.csv") 
