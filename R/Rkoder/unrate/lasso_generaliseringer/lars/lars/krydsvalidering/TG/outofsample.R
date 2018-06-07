source("lasso_generaliseringer/lars/lars/forecastLarsTG.R")
source("lasso_generaliseringer/lars/lars/krydsvalidering/TG/insample.R")

fc = ForecastLarsTG(y, x, idx = 552, getmin_lars$lambda.1se)
