source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)

# antallet af parameter er estimeret til at være 20
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 20)


# Z-score -----------------------------------------------------------------

coef0 = t(larinf$vmat %*% y_train) * (larinf$sign)
coef0/sd
zscore = fixed_lasso_kryds$coef0 /fixed_lasso_kryds$sd


