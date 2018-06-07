source("package.R")
source("data_unrate.R")
source("lasso_generaliseringer/lars/lars/krydsvalidering/insample.R")

lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
lar_coef = coef(lar_fit, s = 20, mode = "step")
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 19, verbose = TRUE)
