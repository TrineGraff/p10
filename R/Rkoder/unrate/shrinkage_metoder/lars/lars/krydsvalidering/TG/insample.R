source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 19, verbose = TRUE)

