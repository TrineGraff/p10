source("/Users/trinegraff/Desktop/Projekt/R/unrate/script/script_lars.R")

parm = function(x) {
  (sum(x != 0))
}

# lasso -------------------------------------------------------------------

min <- min(lars_cv$cv)
idmin = match(min, lars_cv$cv)
l1_min = lars_cv$index[idmin]

se = (lars_cv$cv + lars_cv$cv.error)[idmin]
idse = lars_cv$cv <= se #større eller lig med en standard afvigelse
l1_1se = max(lars_cv$index[idse], na.rm = TRUE)
match(lars_cv$index, l1_1se)

data.frame(
  lambda = c("min", "1se"), 
  vaedi = c(l1_min, l1_1se),
  error = c(lars_cv$cv[idmin], lars_cv$cv[37]),
  p = c(parm(coef(lars_, s = l1_min, mode = "fraction")), parm(coef(lars_, s = l1_1se, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)

b_hat = coef(lars_, s = l1_min, mode = "fraction")
idx_hat = which(b_hat != 0) 
b_hat[idx_hat]

# Elastic net -------------------------------------------------------------

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




