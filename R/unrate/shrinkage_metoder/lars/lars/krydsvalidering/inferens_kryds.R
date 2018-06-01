source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
lar_coef = coef(lar_fit, s = 20, mode = "step")
round(lar_coef[which(coef(lar_fit, s = 20, mode = "step")!= 0)], digits = 4)

#lar_fit$beta hver søjle viser beta koefficienter, hvor colnames er lambda værdierne
#larInf bruger beta værdierne, samt polyhefral selektsion i hvert trin

#antallet af parameter er 19, s_hat = getmin_lars$lambda.1s = 20 
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 19, verbose = TRUE)

#viser punktet \eta * y for hver variable, samt dens trunkerede interval of kondidens intervallet
data.frame(vup = round(larinf $vup, digits = 3), vlo = round(larinf $vlo, digits = 3), larinf $ci)
#vi ser at de er meget tætte på hinanden 



