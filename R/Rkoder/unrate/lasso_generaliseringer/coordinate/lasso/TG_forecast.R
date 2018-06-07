ForecastTG = function(y, x, idx, lambda){
  n = length(y_test)
  m = 4 
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    n.obs = length(Y.res)
    X.out = x[((idx - m) + i), ]
    fit = glmnet(X.in, Y.res, family = "gaussian", alpha = 1, 
                 intercept = FALSE, standardize = FALSE)
    beta = as.vector(coef(fit, s = lambda / n.obs)) %>% .[-1] 
    fixed = fixedLassoInf(X.in, Y.res, beta, lambda = lambda, 
                 intercept = FALSE, alpha = 0.1)
    test = cbind(fixed$vars)
    pred_test = X.out[test] %*% fixed$coef0
    return(pred_test)
  }
  print(fc)
}



