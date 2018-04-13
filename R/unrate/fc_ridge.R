lambda.min = read.csv("results/ridge_lambda.csv") 
source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(glmnet)

x = data_raw[1]
y = scale(data_raw[, "UNRATE"], scale = FALSE) 
# ridge -------------------------------------------------------------------

forecast = function(y, x, idx = idx, lambda) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)]) - 1) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ]
    
    fit = glmnet(x_kov, y_res, alpha = 0, intercept = FALSE)
  }
  print(fit)
}

forecast(y, x, idx, lambda.min$lambda_min) 
