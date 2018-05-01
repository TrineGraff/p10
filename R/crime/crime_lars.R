library(lars)

crime <- read.csv("/Users/louisenygaardchristensen/Documents/AAU/10. semester/crime data/crime.csv")
head(crime)

y <- scale(crime$crime.rate, scale = FALSE)
x <- scale(as.matrix(crime[, -6])) # 5 prædiktorer

# ols ---------------------------------------------------------------------

ols <- lm(y ~ x - 1)
beta_ols <- ols$coef
summary(ols)

# lars-lasso --------------------------------------------------------------

lars.las <- lars(x, y, type = "lasso", trace=TRUE)
plot(lars.las)
# grafen af lasso estimaterne som en fkt af shrinkage illustrerer rækkefølgen
# hvori variablerne enter modellen, idet man ser bort fra betingelsen på L1 normen
# af deres estimater.
# I begyndelsen er der intet i modellen (se til venstre i grafen, hvor s = 0)
# Når vi bevæger os mod højre i grafen, finder vi at den første variabel som enter
# er variabel 1 (funding), derefter variabel 3 (not.hs), variabel 2 (hs),
# variabel 5 (college4) og til slut variabel 4 (college).

# Nedenfor gives parameter estimaterne for de valgte værdier af shrinkage:
# s = 0.25, 0.50, 0.75 og 1.00.
# lasso estimaterne for s = 1 er OLS estimaterne (se summary(ols))

objects(grep("lars", search()))

coef.lars(lars.las)
summary.lars(lars.las)
coef(lars.las, s = c(0.25, 0.50, 0.75, 1.0), mode = "fraction")

cv.lars(x, y, type = "lasso", K = 10)
# Output af krydsvalidering (gennemsnitlige kvadrerede fejl og deres
# standard fejl grænser) viser at den gennemsnit kvadrerede fejl stiger voldsomt hvis
# vi skrinker koefficienterne aggresivt.
# Den gennemsnitlige kvadrerede fejl er mindst doe s = 0.4


lars.las$beta
n <- nrow(x)
betas    <- lars.las$beta
df       <- lars.las$df
MSE      <- lars.las$RSS/n
bic      <- log(n)*df+n*log(MSE)
cbind(df,MSE,bic)
bestb    <- which.min(bic)

plot(bic,cex=2)
points(bestb,bic[bestb],pch=19,cex=2)

beta_lasso <- betas[bestb,]

mean((beta_ols-beta_lasso)^2)


# lar ---------------------------------------------------------------------

lar <- lars(x, y, type="lar", trace=TRUE)
summary(lar)
plot(lar)
coef(lar)

# cv.lars() uses crossvalidation to estimate optimal position in path
cv.lar <- cv.lars(x, y, type="lar", trace = TRUE)
cv.lar

# Use the best Cp value to find best model: 
a <- summary(lar)
# Print out coefficients at optimal s.
coef(lar, s = which.min(a$Cp), mode= "step")

par(mfrow=c(1,2))
plot.lars(lar, xvar = "norm", breaks = TRUE, plottype = "coefficients") #default
plot.lars(lar, xvar = "step", breaks = TRUE, plottype = "Cp")

# lars lasso ---------------------------------------------------------------

lars.lasso <- lars(x, y, type = "lasso", trace = TRUE)
par(mfrow=c(1,2))
plot.lars(lars.lasso, xvar = "norm", breaks = TRUE, plottype = "coefficients") #default
plot.lars(lars.lasso, xvar = "step", breaks = TRUE, plottype = "Cp")

plot.lars(lars.lasso, xvar = "df", breaks = TRUE, plottype = "coefficients")
plot.lars(lars.lasso, xvar = "step", breaks = TRUE, plottype = "coefficients")
plot.lars(lars.lasso, xvar = "arc.length", breaks = TRUE, plottype = "coefficients")
