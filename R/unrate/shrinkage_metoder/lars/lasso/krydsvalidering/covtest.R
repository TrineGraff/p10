source("package.R")
source("shrinkage_metoder/lars/lasso/krydsvalidering/insample.R")

covtest = covTest(lasso_fit, x_train, y_train)

coef = coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")
fit_val = x_train %*% coef

lm = lm(y_train ~ 0 + x_train)
rrs = sum((y_train - lm$fitted.values)^2) /(548-126)
round(sqrt(rrs), digits = 4)

which(coef != 0)

#anvender 21 steps med krydsvalidering
which(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")!=0)

#variablerne 35, 19, 79 tilføjes og fjernes igen
colnames(x_train)[78]

#variablerne 78 tilføjes, fjernes og tilføjes igen
p = (covtest$results[,3])
plot(p)
curve(df(exp, df1=2, df2=422), from=0, to=5)

plot(qexp(p) ,p, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")

plot(df(x = nu, df1 = 2, df2 = 422), type = "l")

plot(qexp(1),nu)
