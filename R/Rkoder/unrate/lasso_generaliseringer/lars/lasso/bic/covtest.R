source("shrinkage_metoder/lars/lasso/bic/insample.R")

covTest(lasso_fit, x_train, y_train)

which(coef(lasso_fit, s = lasso_bic$f_hat, mode = "fraction")!=0)

#variablerne 21, 35, 31,32,19, 79 tilføjes og fjernes igen

colnames(x_train)[79]

#variablen 78 bliver tilføjet, fjernet og tilføjet igen. 