source("data_unrate.R")
library(glmnet)

# Uden reestimering -------------------------------------------------------

pred_test = x_test %*% lasso_fit$beta
x_test
pred = predict(lasso_fit, x_test)
identical(as.vector(pred), as.vector(pred_test)) #true


# Rolling Window forecast -------------------------------------------------

rolling_window_fc = function(y, x, alpha, w_size, lambda ){
  n = length(y_test) # skal igennem data setup_data 
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
  #Rolling window forecast
  x_in = x[i:(w_size + i - 1), ] 
  y_res = y[i:(w_size + i - 1)]
  x_out = x[w_size + i, ]
  
    #Regression Model
  fit = glmnet(x_in, y_res, family = "gaussian", alpha = alpha, 
              intercept = FALSE, standardize = FALSE)
  beta = as.vector(coef(fit, s = lambda) %>% .[-c(1),])
  pred_test = x_out %*% beta

  return(pred_test)
}
print(fc)
}


# Expanding window forecast -----------------------------------------------

expanding_window_fc = function(y, x, alpha, idx, lambda ){
  n = length(y_test)
  
  fc = foreach(i = 1:n, .combine = rbind) %do%{
    #expanding window
    x_in = x[1:(idx + i - 1), ] 
    y_res = y[1:(idx + i - 1)]
    x_out = x[idx + i, ]
    
    # Regression Model #
    fit = glmnet(x_in, y_res, family = "gaussian", alpha = alpha, 
                 intercept = FALSE, standardize = FALSE)
    beta = as.vector(coef(fit, s = lambda) %>% .[-c(1),])
    pred_test = x_out %*% beta
    
    return(pred_test)
  }
  print(fc)
}

# h-step-ahead forecast ---------------------------------------------------

#Eksempel på h-step ahead. Vi skal give de sande værdier af x, 
#men ikke af y hvor vi bruger de prædikterede. 
#giver det mening. 
pred = x_train %*% lasso_fit$beta
pred_5 = x[c((idx +1 ):(idx + 5)), ] %*% lasso_fit$beta

y_trine = rbind(pred, pred_5)
fit_trine = glmnet(x[1:(idx +5),], y_trine, family = "gaussian", alpha = 1, 
                   intercept = FALSE, standardize = FALSE, lambda = lambda_lasso)

pred = x[1:] %*% fit_trine$beta