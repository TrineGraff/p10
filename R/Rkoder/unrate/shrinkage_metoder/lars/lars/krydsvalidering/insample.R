source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/adj.r.2.R")
set.seed(1)

lars_cv = cv.lars(x_train, y_train, type = "lar", intercept = FALSE, 
                   normalize = FALSE, trace = TRUE)
lars_fit = lars(x_train, y_train, type = "lar", intercept = FALSE, 
                normalize = FALSE)
getmin_lars = getmin_l(lars_cv$index, lars_cv$cv, lars_cv$cv.error)


#omregner til fraction
s1 = apply(abs(lars_fit$beta), 1, sum) #summere over rækken. dvs vi får summen af koefficienterne
f = s1/max(s1)

data.frame(
  s = c("min", "1se"), 
  vaedi = c(f[28], f[20]),
  error = c(lars_cv$cv[getmin_lars$idx_min], lars_cv$cv[getmin_lars$idx_1se]),
  p = c(parm(coef(lars_fit, s = f[20], mode = "fraction")), 
        parm(coef(lars_fit, s = f[28], mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)

