source("shrinkage_metoder/lars/lasso/krydsvalidering/insample.R")

covtest = covTest(lasso_fit, x_train, y_train)

