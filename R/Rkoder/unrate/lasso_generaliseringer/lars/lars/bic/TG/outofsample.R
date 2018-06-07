source("lasso_generaliseringer/lars/lars/forecastLarsTG.R")
source("lasso_generaliseringer/lars/lars/bic/TG/insample.R")

fc = ForecastLarsTG(y, x, idx = 552, (lars_bic$p +1))

