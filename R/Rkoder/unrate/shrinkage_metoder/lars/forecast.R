lars_forecast = function(y, x, idx, s, type, mode){
  n = length(y_test)
  m = 4 #antallet af lags
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    #expanding window
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    X.out = x[((idx - m) + i), ]
    # Regression Model #
    fit = lars(X.in, Y.res, type = type, normalize = FALSE, 
                     intercept = FALSE)
    beta = as.vector(coef(fit, s = s, mode = mode))
    pred_test = X.out %*% beta
    
    return(pred_test)
  }
  print(fc)
}

#fc_lars = lars_forecast(y,x, idx, s = 0.26, type = "lasso", mode = "fraction" )
#lars_forecast(y,x, idx, s = 28, type = "lar", mode = "step" )
