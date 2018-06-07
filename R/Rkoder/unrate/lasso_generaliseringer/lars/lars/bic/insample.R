source("package.R")
source("data_unrate.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/bic_lars.R")

lars_fit = lars(x_train, y_train, type = "lar", normalize = FALSE, intercept = FALSE)
lars_bic = larsBIC(y_train, x_train,lars_fit )

coef_1 = coef(lars_fit, s = lars_bic$f_hat, mode = "fraction")
idx_hat_1 = which(coef_1 != 0)
coef_1[idx_hat_1]
