source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/krydsvalidering/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
lar_coef = coef(lar_fit, s = 20, mode = "step")


idx_lar = which(lar_coef != 0)
round(lar_coef[which(coef(lar_fit, s = 20, mode = "step")!= 0)], digits = 4)

#lar_fit$beta hver søjle viser beta koefficienter, hvor colnames er lambda værdierne
#larInf bruger beta værdierne, samt polyhefral selektsion i hvert trin

#antallet af parameter er 19, s_hat = getmin_lars$lambda.1s = 20 
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 19, verbose = TRUE)
#viser punktet \eta * y for hver variable, samt dens trunkerede interval of kondidens intervallet
data.frame(vup = round(larinf $vup, digits = 3), vlo = round(larinf $vlo, digits = 3), larinf $ci)
#vi ser at de er meget tætte på hinanden 
larinf$vmat

coef = c(0.002, 0.004, 0.001, 0.002, -0.001, -0.267, 0.000, -0.003, 0.002, 0.243, -0.006, -0.005,
         0.003, 0.006, -0.005, -0.009, -0.003, 0.003, 0.002)

fit = x_train[, idx_lar] %*%coef 

# Residual plot -----------------------------------------------------------
res = y_train - fit 
res = scale(res)
tmp = data.frame(Date = as.Date(dato_train), y = res)

qqnorm<- qqnorm.plot(res)
hist <- histogdens.plot(res)
resid <- residuals.plot(res)
acf <- residuals.acf.plot(res)
print(grid.arrange(top=textGrob('', gp=gpar(fontsize=20)), hist, qqnorm, resid, acf,
                   layout_matrix = matrix(c(1,2,3,4), ncol = 2, byrow = 2)))

skewness(res)
kurtosis(res)
jarque.bera.test(res)
Box.test(res, lag = 10, "Ljung-Box")




