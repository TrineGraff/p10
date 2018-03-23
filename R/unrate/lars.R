source("/Users/trinegraff/Desktop/Projekt/R/data/setup_data.R")
library(lars)
library(ggplot2)
drops = c("UNRATE")
x = data_train[ , !(colnames(data_train) %in% drops)] 
y = data$UNRATE[1:idx]

parm = function(x) {
  (sum(x != 0))
}
set.seed(109)


# lasso -------------------------------------------------------------------
lars_cv = cv.lars(x, y, type = "lasso", intercept = FALSE, normalize = FALSE, trace = TRUE)
lars_ = lars(x, y, type = "lasso")

min <- min(lars_cv$cv)
idmin = match(min, lars_cv$cv)
l1_min = lars_cv$index[idmin]

se = (lars_cv$cv + lars_cv$cv.error)[idmin]
idse = lars_cv$cv <= se #større eller lig med en standard afvigelse
l1.1se = max(lars_cv$index[idse], na.rm = TRUE)
length(train_dato)
length(lars_cv$index)

# Elastic net -------------------------------------------------------------

# plot --------------------------------------------------------------------

df_la = data.frame(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

ggplot(df_la, aes(df_la$lars_cv.index,df_la$lars_cv.cv)) + 
  geom_errorbar(aes(ymin = df_la$lars_cv.cv + df_la$lars_cv.cv.error, 
                    ymax = df_la$lars_cv.cv - df_la$lars_cv.cv.error, width = .1)) +
  geom_point(col = "red") +
  labs(x = "Fraktion af sidste L1 norm", y = "MSE", color = "") + 
  geom_vline(aes(xintercept= l1_min, col = "blue"), linetype="dotted") +
  geom_vline(aes(xintercept= l1.1se, col = "brown"), linetype="dotted") +
  ggtitle("Lasso") + scale_color_manual(labels = c(expression(lambda[min]), expression(lambda[1][sd])), values = c("blue", "brown"))




