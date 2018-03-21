source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(lars)
library(ggplot2)
drops = c("UNRATE")
x = data_train[ , !(colnames(data_train) %in% drops)] 
y = data$UNRATE[1:idx]

parm = function(x) {
  (sum(x != 0))
}
set.seed(109)


# lasso -------------------------------------------------------------------
lars_cv = cv.lars(x, y, type = "lasso", intercept = FALSE, normalize = FALSE, trace = TRUE)
lars = lars(x, y, type = "lasso")



ideal_l1_ratio <- lasso_cv$index[which.max(lasso_cv$cv - lasso_cv$cv.error <= min(lasso_cv$cv))]
obj <- lars(x, y)
scaled_coefs <- scale(obj$beta, FALSE, 1 / obj$normx)
l1 <- apply(X = scaled_coefs, MARGIN = 1, FUN = function(x) sum(abs(x)))
coef(obj)[which.max(l1 / tail(l1, 1) > ideal_l1_ratio),]

