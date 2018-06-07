source("package.R")
source("data_unrate.R")
source("parm.R")
source("lasso_generaliseringer/lars/bic_lars.R")

lars_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, 
                intercept = FALSE)
lars_bic = larsBIC(y_train, x_train,lars_fit)

