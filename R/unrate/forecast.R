forecast = function(y, x, idx = idx, lambda = lambda, alpha, weights = NULL) {
  fc = c(NA)
  for(k in 0:length(y[-c(1:idx)]) - 1) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den observerede værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = glmnet(x_kov, y_res, alpha = alpha, intercept = FALSE, weights = weights)
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    fc[k+1] = x[dim(x_kov)[1], ] %*% beta_hat #finder række i vores df og ganger på beta_hat
    test = length(x[dim(x_kov)[1],]) 
  }
  print(list("fc" = fc, "test" = test))
}


