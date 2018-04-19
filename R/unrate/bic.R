source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(glmnet)
library(gglasso)

drops = c("UNRATE")
x = scale(data_train[ , !(colnames(data_train) %in% drops)]) 
y = scale(data_train[, "UNRATE"], scale = FALSE)

# bic ---------------------------------------------------------------------

lassoBIC <- function(y, x, fit) {
parm <- function(x) {
    (sum(x != 0))
  }
BIC <- function(y, x, fit, i) {
  n.obs <- length(y)
  sigma2est <- sum((y  - x %*% fit$beta[,i])^2) / n.obs
  BIC <- log(sigma2est) + fit$df[i] * log(n.obs) / n.obs
  return(BIC)
}
n.lambda = length(fit$lambda)
BICs = rep(NA, n.lambda)
for (i in 1:n.lambda) {
  BICs[i] <- BIC(y, x, fit, i)
}
idx = which.min(BICs)
  print(list("bic" = BICs,
          "lambda" = fit$lambda[idx], 
           "bic_min" = BICs[idx], 
           "p" = parm(coef(fit, s = c(fit$lambda[idx])))  
           ))
}


# lasso -------------------------------------------------------------------

fit_lasso = glmnet(x, y, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y, x, fit_lasso)

b_hat = coef(fit_lasso, s = fit_bic_lasso$lambda)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


# ridge -------------------------------------------------------------------

fit_ridge = glmnet(x, y, family = "gaussian", alpha = 0, intercept = FALSE, standardize=FALSE)
fit_bic_ridge = lassoBIC(y, x, fit_ridge)


# Elastic net -------------------------------------------------------------
alpha.grid = seq(0.1, 0.9, length = 10)
alpha_t = c(0.1, 0.189, 0.278, 0.367, 0.456, 0.544, 0.633, 0.722, 0.811, 0.9)

for (i in alpha_t) {
  assign(paste("fit",i , sep=""), glmnet(x, y, alpha=i,family="gaussian", standardize=FALSE, intercept = FALSE))
}

el0.1 = lassoBIC(y,x, fit0.1)
el0.189 = lassoBIC(y,x, fit0.189)
el0.278 = lassoBIC(y,x, fit0.278)
el0.367 = lassoBIC(y,x, fit0.367)
el0.456 = lassoBIC(y,x, fit0.456)
el0.554 = lassoBIC(y,x, fit0.544)
el0.633 = lassoBIC(y,x, fit0.633)
el0.722 = lassoBIC(y,x, fit0.722)
el0.811 = lassoBIC(y,x, fit0.811)
el0.9 = lassoBIC(y,x, fit0.9)

df = data.frame(el0.1$bic, el0.189$bic, el0.278$bic, 
                el0.367$bic, el0.456$bic, el0.554$bic,
                el0.633$bic, el0.722.bic = c(el0.722$bic, rep(NA, 11)), 
                el0.811.bic = c(el0.811$bic, NA), el0.9$bic)
which(df == min(df, na.rm = TRUE), arr.ind = TRUE) #minimum er når alpha = 0.9
el0.9 = lassoBIC(y,x, fit0.9)

b_hat_el = coef(fit0.9, s = el0.9$lambda)
idx_hat_el = which(b_hat_el != 0) 
b_hat_el[idx_hat_el, ]
# adaptive lasso med ols vægte --------------------------------------------

fit_ols = lm(y~0+x)
coef = as.data.frame(fit_ols$coefficients)
weight_ols = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_fit = glmnet(x, y, intercept = FALSE, family = "gaussian", alpha = 1, standardize=FALSE, penalty.factor = weight_ols)

adap_bic = lassoBIC(y, x, adap_ols_fit)

b_hat_ols = coef(adap_ols_fit, s = adap_bic$lambda)
idx_hat_ols = which(b_hat_ols != 0) 
b_hat_ols[idx_hat_ols, ]

# adaptive med lasso vægte ------------------------------------------------
v_l = 1/abs(b_hat[idx_hat, ])
adap_lasso_fit = glmnet(x[,idx_hat -1] , y, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso = lassoBIC(y, x[,idx_hat-1], adap_lasso_fit)

b_hat_adap = coef(adap_lasso_fit, s = adap_lasso$lambda)


# gglasso -----------------------------------------------------------------

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 
gglasso_fit = gglasso(x, y, group = grp, intercept = FALSE)


grp_bic = lassoBIC(y, x, gglasso_fit)

idx.bic_grp = which.min(grp_bic)
lambda.opt.grp = gglasso_fit$lambda[idx.bic_grp]
data.frame(
  lambda_val = c(lambda.opt.grp),
  BIC = c(grp_bic[idx.bic]),  
  p = parm(coef(gglasso_fit, s = c(lambda.opt.grp)))
) 
