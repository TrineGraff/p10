source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)

# antallet af parameter er estimeret til at være 20
larInf(lar_fit, alpha = 0.1, type = "all", k = 20)
colnames(x_train)[21]

lars_fit = lars(x_train, y_train, type = "lar", normalize = FALSE, intercept = FALSE)
test = coef(lars_fit, s = 21)
idx = which(test != 0)
data.frame(round(test[idx], digits = 3), idx)
