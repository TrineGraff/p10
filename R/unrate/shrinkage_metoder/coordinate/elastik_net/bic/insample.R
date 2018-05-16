source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/bic.R")
set.seed(1)

alpha.grid = round(seq(0, 1, length = 10), digits = 2)
for (i in alpha.grid) {
  assign(paste("fit",i , sep=""),  glmnet(x_train, y_train, alpha=i,family="gaussian", standardize=FALSE, intercept = FALSE))
}

el0 = lassoBIC(y_train, x_train, fit0)
el0.11 = lassoBIC(y_train, x_train, fit0.11)
el0.22 = lassoBIC(y_train, x_train, fit0.22)
el0.33 = lassoBIC(y_train, x_train, fit0.33)
el0.44 = lassoBIC(y_train, x_train, fit0.44)
el0.56 = lassoBIC(y_train, x_train, fit0.56)
el0.67 = lassoBIC(y_train, x_train, fit0.67)
el0.78 = lassoBIC(y_train, x_train, fit0.78)
el0.89 = lassoBIC(y_train, x_train, fit0.89)
el1 = lassoBIC(y_train, x_train, fit1)

df = data.frame(el0$bic_min, el0.11$bic_min, el0.22$bic_min, 
                el0.33$bic_min, el0.44$bic_min, el0.56$bic_min, 
                el0.67$bic_min, el0.78$bic_min, el0.89$bic_min, el1$bic_min)
which.min(df) # vælger lasso

