source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(glmnet)
library(gglasso)

drops = c("UNRATE")
x = data_train[ , !(colnames(data_train) %in% drops)] 
y = data$UNRATE[1:idx]

parm = function(x) {
  (sum(x != 0))
}


# bic ---------------------------------------------------------------------

lassoBIC <- function(y, x, fit) {
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
return(BICs)
}



# lasso -------------------------------------------------------------------

fit = glmnet(x, y, family = "gaussian", alpha = 1, intercept = FALSE, standardize=FALSE)
fit_bic_lasso = lassoBIC(y, x, fit)

idx_min = which.min(fit_bic_lasso)
lambda.opt = fit$lambda[idx_min]

b_hat = coef(fit, s = lambda.opt)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


# ridge -------------------------------------------------------------------

fit_ridge = glmnet(x, y, family = "gaussian", alpha = 0, intercept = FALSE, standardize=FALSE)
fit_bic_ridge = lassoBIC(y, x, fit_ridge)

idx_min = which.min(fit_bic_ridge)
lambda.opt = fit$lambda[idx_min]

b_hat = coef(fit, s = lambda.opt)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]

# Elastic net -------------------------------------------------------------
alpha.grid = seq(0.1, 0.9, length = 10)
alpha_t = c(0.1, 0.189, 0.278, 0.367, 0.456, 0.544, 0.633, 0.722, 0.811, 0.9)

for (i in alpha_t) {
  assign(paste("fit",i , sep=""), glmnet(x, y, alpha=i,family="gaussian", standardize=FALSE))
}

glmnet(x,y, alpha = 0.1)

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

df = data.frame(el0.1, el0.189, el0.278, el0.367, el0.456, el0.554, el0.633, el0.722 = c(el0.722, rep(NA, 11)), c(el0.811, NA), el0.9)
which(df == min(df, na.rm = TRUE), arr.ind = TRUE) #minimum er når alpha = 0.9

idx_el = which.min(df$el0.9)
lambda.opt = fit0.9$lambda[idx_el]
parm(coef(fit0.9, s = lambda.opt ))

# adaptive lasso med ols vægte --------------------------------------------

fit_ols = lm(y~0+x)
coef = as.data.frame(fit_ols$coefficients)
v = 1/abs(coef$`fit_ols$coefficients`)

adap_ols_fit = glmnet(x, y, intercept = FALSE, family = "gaussian", alpha = 1, standardize=FALSE, penalty.factor = v)
bic_adap = lassoBIC(y, x, adap_ols_fit)
idx_adap = which.min(bic_adap)

lambda.opt.ad = adap_ols_fit$lambda[idx_adap]
parm(coef(adap_ols_fit, s = lambda.opt.ad ))


# adaptive med lasso vægte ------------------------------------------------
v_l = 1/abs(b_hat[idx_hat -1, ]) #intercept er inkluderet



adap_lasso_fit = glmnet(x[,idx_hat-1], y, intercept = FALSE, 
                        family = "gaussian", alpha = 1, standardize=FALSE, 
                        penalty.factor = v_l)

adap_lasso = lassoBIC(y, x[,idx_hat-1], adap_lasso_fit)

idx_adl = which.min(adap_lasso)
lambda.opt.ad.la = adap_lasso_fit$lambda[idx_adl]
coef(adap_lasso_fit, s = lambda.opt.ad.la)



# gglasso -----------------------------------------------------------------

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4)) 
gglasso_fit = gglasso(x, y, group = grp)


grp_bic = lassoBIC(y, x, gglasso_fit)

idx.bic = which.min(grp_bic)
lambda.opt.grp = gglasso_fit$lambda[idx.bic]
length(coef(gglasso_fit, s = lambda.opt.grp))

