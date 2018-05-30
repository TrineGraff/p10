source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)

#antallet af parameter er 19, s_hat = getmin_lars$lambda.1s = 20 
TG = larInf(lar_fit, alpha = 0.1)

?larInf
TG$pv
pv = which(TG$pv <= 0.05)
pv_test = round(TG$pv, digits = 3)
pv_0.05 = which(pv_test <= 0.05)

#[1]   1   2   4  12  15  66  69  71  79  80  85  89  95 103 105, 

#variablerne p-værdierne < 0.05 tilhører. 
# 21, 27, 35, 78, 80, 48, 81, 85, 117, 65, 96, 63, 82, 38

colnames(x_train)[80]
