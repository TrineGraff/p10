source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")

lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
idx_lar = larinf$vars
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 20)
