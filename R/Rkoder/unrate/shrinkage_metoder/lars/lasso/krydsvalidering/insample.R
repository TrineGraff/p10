source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

lasso_cv = cv.lars(x_train, y_train, type = "lasso", intercept = FALSE, 
                   normalize = FALSE, trace = FALSE)

lasso_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, intercept = FALSE)
getmin = getmin_l(lasso_cv$index, lasso_cv$cv, lasso_cv$cv.error)

data.frame(
  lambda = c("min", "1se"), 
  vaedi = c(getmin$lambda.min, getmin$lambda.1se),
  error = c(lasso_cv$cv[getmin$idx_min], lasso_cv$cv[getmin$idx_1se]),
  p = c(parm(coef(lasso_fit, s = getmin$lambda.min, mode = "fraction")), 
        parm(coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)
