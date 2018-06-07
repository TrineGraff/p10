ForecastLarsTG = function(y, x, idx, lambda){
  n = length(y_test)
  m = 4 #antallet af lags
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    #expanding window
    print(i)
    X.in = x[1:((idx - m) + i - 1), ] 
    Y.res = y[(m + 1):(idx + i - 1)]
    n.obs = length(Y.res)
    X.out = x[((idx - m) + i), ]
    # Regression Model #
    fit = lar(X.in, Y.res, normalize = FALSE, intercept = FALSE)
    larinf = larInf(fit, alpha = 0.1, type = "all", k = lambda - 1, verbose = TRUE)
    eta = larinf$vmat
    y_lar = larinf$y
    s = larinf$sign
    beta = s * (eta %*% y_lar)
    pred_test = X.out[larinf$vars] %*% beta
    print(pred_test)
    return(pred_test)
  }
  print(fc)
}