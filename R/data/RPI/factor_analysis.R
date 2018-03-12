source("setup_data.R")
library(pls)
library(plsdof)
data_train = data_train[, -c(1,2)]
data = data[, -c(1,2)]
#model selektion for PC regression baseret på cross-validering

pcr = pcr(RPI ~.,data = data, validation = "CV")
summary(pcr)
validationplot(pcr)
pcr$loadings
predplot(pcr)
coefplot(pcr)

data_train_1 = data_train[, -c(1, 2,3)]
data_train_1 = as.matrix(data_train_1)
pcr_cv = pcr.cv(data_train_1, y, k = 10, plot.it = TRUE)

#optimal antal af komponenter baseret på mean squared error
set.seed(1)
pcr_cv$m.opt
pcr_cv$intercept
pcr_cv$coefficients
pcr_beta_cv = c(pcr_cv$intercept, pcr_cv$coefficients)

min(pcr_cv$cv.error)
pcr_cv$m.opt #miniumum fejl

