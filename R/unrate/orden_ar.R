# orden p -----------------------------------------------------------------
#Vi bestemmer orden af vores træningsmængde 
source("data_unrate.R")

x.lag = function(x, p.max){
  BIC.vektor = rep(NA, p.max)
  
  for (p in 1:p.max){
    y = x[(p.max + 1):length(x)]
    n.obs = length(y)
    x.lag = matrix(nrow = n.obs, ncol = p)
    for (j in 1:p){
      for (i in 1:n.obs){
        x.lag[i,j] = x[p+i-j]
      }
    }
    beta.hat = solve(crossprod(x.lag), crossprod(x.lag, y))
    sigma2.hat = mean((y - x.lag %*% beta.hat)^2)
    BIC = log(sigma2.hat) + (p * log(n.obs)/n.obs)
    BIC.vektor[p] = BIC
  }
  print(which.min(BIC.vektor))
  #print(BIC.vektor)
}

test = x.lag(y_train, 12)


#x.lag_beta =  [2,]  0.17571061
#[3,]  0.16725550
#[4,]  0.02741658
#[5,]  0.03184849
#[6,] -0.03656064
#[7,]  0.02113470
#[8,]  0.01992752
#[9,] -0.05740958
#[10,]  0.06286115
#[11,] -0.13227739

# Beta print --------------------------------------------------------------

# Udfra overstående funktion er p bestemt. 
# Vi finder beta baseret på træningsmængden
beta = function(x, p) {
  y = x[(p + 1):length(x)]
  
  n.obs = length(y)
  
  x_lag = matrix(nrow = n.obs, ncol = p)
  for (j in 1:p){
    for (i in 1:n.obs){
      x_lag[i,j] = x[p+i-j]
    }
  }
  beta_hat = solve(crossprod(x_lag), crossprod(x_lag, y))
  sigma2.hat = mean((y- x_lag %*% beta_hat)^2)
  
  print(list("beta_hat" = beta_hat, "x.lag" = x.lag))
}

x_est = beta(train, 1)

