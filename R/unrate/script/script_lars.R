source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(lars)
library(ggplot2)
library(elasticnet)
set.seed(109)

drops = c("UNRATE")
x = scale(data_train[ , !(colnames(data_train) %in% drops)]) 
y = scale(data_train[, "UNRATE"], scale = FALSE)

lars_cv = cv.lars(x, y, type = "lasso", intercept = FALSE, normalize = FALSE, trace = TRUE)
lars_ = lars(x, y, type = "lasso")