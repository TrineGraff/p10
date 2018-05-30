source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
#lar_fit$beta hver søjle viser beta koefficienter, hvor colnames er lambda værdierne
#larInf bruger beta værdierne, samt polyhefral selektsion i hvert trin
lar_fit$Gamma
?lar
#antallet af parameter er 19, s_hat = getmin_lars$lambda.1s = 20 
b = larInf(lar_fit, alpha = 0.1, type = "all", k = 19, verbose = TRUE)

#viser punktet \eta * y for hver variable, samt dens trunkerede interval of kondidens intervallet
data.frame(etay = round((b$vmat) %*% b$y, digits =5), vup = round(b$vup, digits = 5), vlo = round(b$vlo, digits = 5), b$ci)
#vi ser at de er meget tætte på hinanden 

0.0093068023 -1.1736194274 -0.8095291767 -0.5683828969 -0.3227477837 



fit0=coxph(Surv(y,status)~x[,m])
coef0=fit0$coef
se0=sqrt(diag(fit0$var))
zscore0=coef0/se0


# JT: these are not the one step estimators but they are close
fit0=glm(y~x[,m],family="gaussian")
sfit0=summary(fit0)
coef0=bbar[-1]        #fit0$coef[-1]
se0=sqrt(diag(MM)[-1]) # sfit0$cov.scaled)[-1])
zscore0=coef0/se0


fit0=glm(y_train~ 0 + x_train[,21],family="gaussian")
sfit0 = summary(fit0)
coef0 = fit0$coef + mean(x_train[,21])
se0 = sqrt(diag(sfit0$cov.scaled))
zscore = coef0/se0

sd = sd(x_train[,21])*sqrt((length(x_train[,21])-1)/(length(x_train[,21
                                                               ])))

0.002 - mean(x_train)/sd

TG$vup
pv = which(TG$pv <= 0.05)
pv_test = round(TG$pv, digits = 3)
pv_0.05 = which(pv_test <= 0.05)

#[1]   1   2   4  12  15  66  69  71  79  80  85  89  95 103 105, 

#variablerne p-værdierne < 0.05 tilhører. 
# 21, 27, 35, 78, 80, 48, 81, 85, 117, 65, 96, 63, 82, 38

colnames(x_train)[80]
