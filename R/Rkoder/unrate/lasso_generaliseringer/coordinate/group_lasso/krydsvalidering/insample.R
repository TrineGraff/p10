source("package.R")
source("parm.R")
source("data_unrate.R")
set.seed(1)

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 

gglasso_fit = gglasso(x_train, y_train, group = grp, dfmax = 8, 
                      intercept = FALSE, loss = "ls")
gglasso_cv <- cv.gglasso(x_train, y_train, group = grp, 
                         nfold = 10, intercept = FALSE, loss = "ls" )

data.frame(
  lambda = c("min", "1se"), 
  lambda_val = c(log(gglasso_cv$lambda.min), log(gglasso_cv$lambda.1se)),
  error = with(gglasso_cv, c(cvm[which(lambda == gglasso_cv$lambda.min)], cvm[which(lambda == gglasso_cv$lambda.1se)])),  
  p = apply(coef(gglasso_fit , s = c(gglasso_cv$lambda.min, gglasso_cv$lambda.1se)), 2, parm) 
) 



