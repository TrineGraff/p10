source("lasso_generaliseringer/lars/lars/bic/insample.R")

lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = lars_bic$p )
