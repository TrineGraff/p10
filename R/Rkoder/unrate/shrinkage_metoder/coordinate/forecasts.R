Forecast = function(y, x, alpha, idx, lambda){
  n = length(y_test)
  m = 4 #antallet af lags

  fc = foreach(i = 1:n, .combine = rbind) %do%{
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    X.out = x[((idx - m) + i), ]
    fit = glmnet(X.in, Y.res, family = "gaussian", alpha = alpha, 
                 intercept = FALSE, standardize = FALSE)
    beta = as.vector(coef(fit, s = lambda)) %>% .[-1] 
    pred_test = X.out %*% beta

    return(pred_test)
  }
  print(fc)
}


gglasso_forecast = function(y, x, idx, lambda, group ){
  n = length(y_test)
  m = 4 #antallet af lags
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    #expanding window
    X.in = x[1:((idx - m) + i - 1), ] %>% as.matrix(.)
    Y.res = y[(m + 1):(idx + i - 1)]
    X.out = x[((idx - m) + i), ]
    # Regression Model #
    #fit = glmnet(X.in, Y.res, family = "gaussian", alpha = alpha, 
    #             intercept = FALSE, standardize = FALSE)
    fit = gglasso(X.in, Y.res, group = grp, intercept = FALSE, loss = "ls")
    beta = as.vector(coef(fit, s = lambda)) %>% .[-1] 
    pred_test = X.out %*% beta
    
    return(pred_test)
  }
  print(fc)
}

adap_forecast = function(y, x, alpha, idx, lambda, w){
  n = length(y_test)
  m = 4 #antallet af lags
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    #expanding window
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    X.out = x[((idx - m) + i), ]
    # Regression Model #
    fit = glmnet(X.in, Y.res, family = "gaussian", alpha = alpha, 
                 intercept = FALSE, standardize = FALSE, penalty.factor = w)
    beta = as.vector(coef(fit, s = lambda)) %>% .[-1] 
    pred_test = X.out %*% beta
    
    return(pred_test)
  }
  print(fc)
}



