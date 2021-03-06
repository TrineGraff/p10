source("data_unrate.R")
source("package.R")
source("parm.R")
set.seed(1)

alpha.grid = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

for (i in alpha.grid) {
  assign(paste("fit", i, sep = ""), 
         cv.glmnet(x_train, y_train, alpha = i, family = "gaussian",
                   standardize = FALSE, intercept = FALSE))
}

cv1_1se  = c(lambda = fit0$lambda.1se, cvm = fit0$cvm[fit0$lambda == fit0$lambda.1se], alpha = 0)
cv2_1se  = c(lambda = fit0.1$lambda.1se, cvm = fit0.1$cvm[fit0.1$lambda == fit0.1$lambda.1se], alpha = 0.1)
cv3_1se  = c(lambda = fit0.2$lambda.1se, cvm = fit0.2$cvm[fit0.2$lambda == fit0.2$lambda.1se], alpha = 0.2)
cv4_1se  = c(lambda = fit0.3$lambda.1se, cvm = fit0.3$cvm[fit0.3$lambda == fit0.3$lambda.1se], alpha = 0.3)
cv5_1se  = c(lambda = fit0.4$lambda.1se, cvm = fit0.4$cvm[fit0.4$lambda == fit0.4$lambda.1se], alpha = 0.4)
cv6_1se  = c(lambda = fit0.5$lambda.1se, cvm = fit0.5$cvm[fit0.5$lambda == fit0.5$lambda.1se], alpha = 0.5)
cv7_1se  = c(lambda = fit0.6$lambda.1se, cvm = fit0.6$cvm[fit0.6$lambda == fit0.6$lambda.1se], alpha = 0.6)
cv8_1se  = c(lambda = fit0.7$lambda.1se, cvm = fit0.7$cvm[fit0.7$lambda == fit0.7$lambda.1se], alpha = 0.7)
cv9_1se  = c(lambda = fit0.8$lambda.1se, cvm = fit0.8$cvm[fit0.8$lambda == fit0.8$lambda.1se], alpha = 0.8)
cv9_1se  = c(lambda = fit0.9$lambda.1se, cvm = fit0.9$cvm[fit0.9$lambda == fit0.9$lambda.1se], alpha = 0.9)
cv10_1se  = c(lambda = fit1$lambda.1se, cvm = fit1$cvm[fit1$lambda == fit1$lambda.1se], alpha = 1)

cv_1se = data.frame(cv1_1se, cv2_1se, cv3_1se, cv4_1se, cv5_1se, cv6_1se, cv7_1se, cv8_1se, cv9_1se, cv10_1se)
which.min(cv_1se[2,])

cv1_min  = c(lambda = fit0$lambda.min, cvm = fit0$cvm[fit0$lambda == fit0$lambda.min], alpha = 0)
cv2_min  = c(lambda = fit0.1$lambda.min, cvm = fit0.1$cvm[fit0.1$lambda == fit0.1$lambda.min], alpha = 0.1)
cv3_min  = c(lambda = fit0.2$lambda.min, cvm = fit0.2$cvm[fit0.2$lambda == fit0.2$lambda.min], alpha = 0.2)
cv4_min  = c(lambda = fit0.3$lambda.min, cvm = fit0.3$cvm[fit0.3$lambda == fit0.3$lambda.min], alpha = 0.3)
cv5_min  = c(lambda = fit0.4$lambda.min, cvm = fit0.4$cvm[fit0.4$lambda == fit0.4$lambda.min], alpha = 0.4)
cv6_min  = c(lambda = fit0.5$lambda.min, cvm = fit0.5$cvm[fit0.5$lambda == fit0.5$lambda.min], alpha = 0.5)
cv7_min  = c(lambda = fit0.6$lambda.min, cvm = fit0.6$cvm[fit0.6$lambda == fit0.6$lambda.min], alpha = 0.6)
cv8_min  = c(lambda = fit0.7$lambda.min, cvm = fit0.7$cvm[fit0.7$lambda == fit0.7$lambda.min], alpha = 0.7)
cv9_min  = c(lambda = fit0.8$lambda.min, cvm = fit0.8$cvm[fit0.8$lambda == fit0.8$lambda.min], alpha = 0.8)
cv9_min  = c(lambda = fit0.9$lambda.min, cvm = fit0.9$cvm[fit0.9$lambda == fit0.9$lambda.min], alpha = 0.9)
cv10_min  = c(lambda = fit1$lambda.min, cvm = fit1$cvm[fit1$lambda == fit1$lambda.min], alpha = 1)

cv_min = data.frame(cv1_min, cv2_min, cv3_min, cv4_min, cv5_min, 
                    cv6_min, cv7_min, cv8_min, cv9_min, cv10_min)
which.min(cv_min[2,])
