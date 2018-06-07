ForecastLARS = function(y, x, idx, s, type, mode){
  n = length(y_test)
  m = 4 
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    X.out = x[((idx - m) + i), ]
    fit = lars(X.in, Y.res, type = type, normalize = FALSE, 
                     intercept = FALSE)
    beta = as.vector(coef(fit, s = s, mode = mode))
    pred_test = X.out %*% beta
    
    return(pred_test)
  }
  print(fc)
}
