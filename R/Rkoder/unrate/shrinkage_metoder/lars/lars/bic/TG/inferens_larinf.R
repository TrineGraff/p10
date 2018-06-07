source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")

#lar fit er det samme, som ved anvendels af lars
lar_fit = lar(x_train, y_train, normalize = FALSE, intercept = FALSE)
idx_lar = larinf$vars
# antallet af parameter er estimeret til at være 20
larinf = larInf(lar_fit, alpha = 0.1, type = "all", k = 20)


coef = c(0.002, 0.004, 0.001, 0.003, -0.001, -0.267, 0.000, -0.004, 0.002, 0.243,
         -0.006, -0.006, 0.003, 0.007, -0.006, -0.009, -0.002, 0.003, 0.001, -0.002)


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





# Z-score -----------------------------------------------------------------

coef0 = t(larinf$vmat %*% y_train) * (larinf$sign)
coef0/sd
zscore = fixed_lasso_kryds$coef0 /fixed_lasso_kryds$sd


