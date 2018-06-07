source("package.R")
source("parm.R")
source("data_unrate.R")
source("lasso_generaliseringer/coordinate/bic.R")

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 

gglasso_fit = gglasso(x_train, y_train, group = grp, 
                      intercept = FALSE, loss = "ls")

grp_bic = lassoBIC(y_train, x_train, gglasso_fit)
