library(lars)

# data med n = 442 og 3 variable, som anvendes i Efrons paper
data("diabetes")
head(diabetes)

# cv.lars -----------------------------------------------------------------

attach(diabetes)
x # matrix med 10 kolonner
y # numerisk vektor
x2 # matrix med 64 kolonner 

cv.lars(x2, y, trace = TRUE, max.steps = 80)
detach(diabetes)

# lars --------------------------------------------------------------------

par(mfrow=c(2,2))
attach(diabetes)
object <- lars(x,y)
plot(object)
object2 <- lars(x,y,type="lar")
plot(object2)
object3 <- lars(x,y,type="for") # Can use abbreviations
plot(object3)
detach(diabetes)

# plot.lars ---------------------------------------------------------------

attach(diabetes)
object <- lars(x,y)
plot(object)
detach(diabetes)
# samme plot som ovenfor med object2


plot.lars(object, xvar = "norm", plottype = "coefficients") # default
plot.lars(object, xvar = "df", breaks = TRUE, plottype = "coefficients")
plot.lars(object, xvar = "step", breaks = TRUE, plottype = "coefficients")
plot.lars(object, xvar = "arc.length", breaks = TRUE, plottype = "coefficients")

plot.lars(object, xvar = "step", breaks = TRUE, plottype = "Cp")


# Predict.lars ------------------------------------------------------------

attach(diabetes)
object <- lars(x, y, type="lasso")
### make predictions at the values in x, at each of the
### steps produced in object
fits <- predict.lars(object, x, type="fit")
### extract the coefficient vector with L1 norm=4.1
coef4.1 <- coef(object, s=4.1, mode="norm") # or
coef4.1 <- predict(object, s=4.1, type="coef", mode="norm")
detach(diabetes)


# Summary.lars ------------------------------------------------------------

attach(diabetes)
object <- lars(x,y)
summary(object)
detach(diabetes)
