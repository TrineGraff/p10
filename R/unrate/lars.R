source("script/script_lars.R")
library(lars)
library(ggplot2)
library(elasticnet)

drops = c("UNRATE")
x = scale(data_train[ , !(colnames(data_train) %in% drops)]) 
y = scale(data_train[, "UNRATE"], scale = FALSE)

parm = function(x) {
  (sum(x != 0))
}

#getanywhere(getmin) - pakke glmnet. 
#vi vil gerne minimum og den med én standardafvigelse
getmin = function (lambda, cvm, cvsd) 
{
  cvmin = min(cvm, na.rm = TRUE)
  idmin = cvm <= cvmin
  lambda.min = max(lambda[idmin], na.rm = TRUE)
  idmin = match(lambda.min, lambda)
  semin = (cvm + cvsd)[idmin]
  idmin = cvm <= semin
  lambda.1se = max(lambda[idmin], na.rm = TRUE)
  list(lambda.min = lambda.min, lambda.1se = lambda.1se,
       idx_1se = match(lambda.1se, lambda), idx_min = match(lambda.min, lambda))
}


# lasso -------------------------------------------------------------------
getmin_l = getmin(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

data.frame(
  lambda = c("min", "1se"), 
  vaedi = c(getmin_l$lambda.min, getmin_l$lambda.1se),
  error = c(lars_cv$cv[getmin_l$idx_min], lars_cv$cv[getmin_l$idx_1se]),
  p = c(parm(coef(lars_, s = getmin_l$lambda.min, mode = "fraction")), 
        parm(coef(lars_, s = getmin_l$lambda.1se, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)


beta_hat = coef(lars_, s = getmin_l$lambda.min, mode = "fraction")
which(beta_hat != 0)
predict(lars_, s = l1_min, mode = "fraction", type = "coefficients")$coefficients)


# plot --------------------------------------------------------------------

df_la = data.frame(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

ggplot(df_la, aes(df_la$lars_cv.index,df_la$lars_cv.cv)) + 
  geom_errorbar(aes(ymin = df_la$lars_cv.cv + df_la$lars_cv.cv.error, 
                    ymax = df_la$lars_cv.cv - df_la$lars_cv.cv.error, width = .1)) +
  geom_point(col = "red") +
  labs(x = "Fraktion af sidste L1 norm", y = "MSE", color = "") + 
  geom_vline(aes(xintercept= l1_min, col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= l1_1se, col = "brown"), linetype="dotted") +
  ggtitle("Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))


# Elastic net -------------------------------------------------------------

#hvilken lambda_2 har den mindste krydsvalideringsfejl. 
min_0.01 = min(fit0.01$cv)
min_0.1 = min(fit0.1$cv)
min_1 = min(fit1$cv)
min_10 = min(fit10$cv)
min_100 = min(fit100$cv)
min = which.min(c(min_0.01, min_0.1, min_1, min_10, min_100))

#finder det s med mindst krydsvaliderings fejl
getmin_en = getmin(fit0.01$s, fit0.01$cv, fit0.01$cv.error)

enet = enet(x, y, lambda = 0.01, intercept= FALSE, normalize = FALSE)



