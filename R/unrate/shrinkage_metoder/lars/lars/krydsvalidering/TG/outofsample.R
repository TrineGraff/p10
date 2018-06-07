source("shrinkage_metoder/lars/lars/forecastLarsTG.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/TG/insample.R")

fc = ForecastLarsTG(y, x, idx = 552, getmin_lars$lambda.1se)

c = tidy(fc)
write.csv(c, file = "lars_TG_kryds.csv") 
