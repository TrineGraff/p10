source("data_unrate.R")
source("package.R")
source("parm.R")
source("shrinkage_metoder/res_plot.R")
source("shrinkage_metoder/lars/getmin.R")
set.seed(1)

lars_cv = cv.lars(x_train, y_train, type = "lar", intercept = FALSE, 
                   normalize = FALSE, trace = TRUE)
lars_fit = lars(x_train, y_train, type = "lar", intercept = FALSE, 
                normalize = FALSE)
getmin_lars = getmin_l(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

s1 = apply(abs(lars_fit$beta), 1, sum) #summere over rækken. dvs vi får summen af koefficienterne
f = s1/max(s1)
