source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(lars)
library(ggplot2)
library(elasticnet)
set.seed(109)

drops = c("UNRATE")
x = scale(data_train[ , !(colnames(data_train) %in% drops)]) 
y = scale(data_train[, "UNRATE"], scale = FALSE)


# lasso --------------------------------------------------------------------

lars_cv = cv.lars(x, y, type = "lasso", intercept = FALSE, normalize = FALSE, trace = TRUE)
lars_ = lars(x, y, type = "lasso")


# elastik net -------------------------------------------------------------

lambda_2.grid = c(0.01, 0.1, 1, 10, 100)

for (i in lambda_2.grid) {
  assign(paste("fit",i , sep=""), cv.enet(x, y, lambda = i, s =seq(0.1, 1, length = 100),
                                          mode = "fraction", trace = TRUE))
}



# adaptive lasso med ols vægte --------------------------------------------
ols_est = lm(y ~ 0 + x)         
coef = as.data.frame(ols_est$coefficients)
v = 1/abs(coef$`ols_est$coefficients`)                    
x_scale = scale(x, center=FALSE, scale = v)
cv.ols = cv.lars(x_scale, y, type="lasso", normalize=FALSE, intercept = FALSE)


# adaptive lasso med lasso vægte ------------------------------------------

#lassos lambda
b_hat = predict(lars_, s = 0.3030303, mode = "fraction", type = "coefficients")$coefficients

idx_hat = which(b_hat != 0)
b_hat[idx_hat]
v_l = 1/abs(b_hat[idx_hat]) #intercept er inkluderet
x_scale_las = scale(x[, idx_hat], center=FALSE, scale = v_l) 

cv.adap.l = cv.lars(x_scale_las, y, type = "lasso", normalize = FALSE, intercept = FALSE)
