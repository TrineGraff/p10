source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/bic_lars.R")

lasso_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, 
                 intercept = FALSE)
lasso_bic = lassoBIC(y_train, x_train, lasso_fit)
