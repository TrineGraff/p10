source("lasso_generaliseringer/lars/lasso/krydsvalidering/insample.R")

covtest = covTest(lasso_fit, x_train, y_train)
