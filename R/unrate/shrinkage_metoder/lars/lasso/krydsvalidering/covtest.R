source("shrinkage_metoder/lars/lasso/krydsvalidering/insample.R")

covTest(lasso_fit, x_train, y_train)

#anvender 21 steps med krydsvalidering
which(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")!=0)

#variablerne 35, 19, 79 tilføjes og fjernes igen
colnames(x_train)[78]

#variablerne 78 tilføjes, fjernes og tilføjes igen