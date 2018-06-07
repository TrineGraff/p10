source("package.R")
source("data_unrate.R")
source("parm.R")
source("lasso_generaliseringer/coordinate/bic.R")
set.seed(1)

alpha.grid = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
for (i in alpha.grid) {
  assign(paste("fit",i , sep=""),  
         glmnet(x_train, y_train, alpha = i,
                family = "gaussian", standardize = FALSE, intercept = FALSE))
}

el0 = lassoBIC(y_train, x_train, fit0)
el0.1 = lassoBIC(y_train, x_train, fit0.1)
el0.2 = lassoBIC(y_train, x_train, fit0.2)
el0.3 = lassoBIC(y_train, x_train, fit0.3)
el0.4 = lassoBIC(y_train, x_train, fit0.4)
el0.5 = lassoBIC(y_train, x_train, fit0.5)
el0.6 = lassoBIC(y_train, x_train, fit0.6)
el0.7 = lassoBIC(y_train, x_train, fit0.7)
el0.8 = lassoBIC(y_train, x_train, fit0.8)
el0.9 = lassoBIC(y_train, x_train, fit0.9)
el1 = lassoBIC(y_train, x_train, fit1)

df = data.frame(el0$bic_min, el0.1$bic_min, el0.2$bic_min, 
                el0.3$bic_min, el0.4$bic_min, el0.5$bic_min, 
                el0.6$bic_min, el0.7$bic_min, el0.8$bic_min, 
                el0.9$bic_min, el1$bic_min)
which.min(df) # vælger lasso

