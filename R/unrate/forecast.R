source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
setwd("~/Desktop/Projekt/R/unrate")

lars_cv = read.csv("results/lars_cv.csv")
beta = read.csv("results/result_lars.csv")

drops = c("UNRATE")
x = data_train[ , !(colnames(data_train) %in% drops)] 
y = data$UNRATE[1:idx]

parm = function(x) {
  (sum(x != 0))
}

# insample ----------------------------------------------------------------
min <- min(lars_cv$cv)
idmin = match(min, lars_cv$cv)
l1_min = lars_cv$index[idmin]


  n.obs = length(y)
  h = 1
  fc.y = y[(1+h):n.obs] #fjerner en observation
  fc.vars = as.matrix(x[(1:n.obs - h), ]) #fjerner en observation
  lambda.opt = min
  fac= as.data.frame(beta)
  f = which(beta_opt != 0) 

Lasso_fc(x,y,1)
