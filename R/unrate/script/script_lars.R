source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(lars)
library(ggplot2)
library(elasticnet)
set.seed(109)

drops = c("UNRATE")
x = scale(data_train[ , !(colnames(data_train) %in% drops)]) 
y = scale(data_train[, "UNRATE"], scale = FALSE)


# lasso --------------------------------------------------------------------

lars_cv = cv.lars(x, y, type = "lasso", intercept = FALSE, normalize = FALSE, trace = TRUE)
lars_ = lars(x, y, type = "lasso")


# elastik net -------------------------------------------------------------

lambda_2.grid = c(0.01, 0.1, 1, 10, 100)

for (i in lambda_2.grid) {
  assign(paste("fit",i , sep=""), cv.enet(x, y, lambda = i, s =seq(0.1, 1, length = 100),
                                          mode = "fraction", trace = TRUE))
}

