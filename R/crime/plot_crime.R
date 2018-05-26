library(plotmo) 
library(glmnet)
library(ggfortify)
library(gridExtra)

crime <- read.csv("../crime/crime.csv")
crime = as.matrix(data.frame(crime))

y = crime$crime.rate
x = data.frame(crime$funding, crime$hs, crime$not.hs, crime$college, crime$college4)

glmnet(x, y, family = "gaussian", alpha = 1)
fit_lasso = glmnet(crime.rate ~ .,data = crime)


fit_el = glmnet(crime.rate ~ ., data = crime, alpha = 0.7)
fit_ridge = glmnet(crime.rate ~ ., data = crime, alpha = 0)



plot_lasso = autoplot(fit_lasso, xvar = "lambda") + 
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle("Lasso")

plot_el = autoplot(fit_el, xvar = "lambda") +
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle(expression(paste("Eleatik net, ", alpha, '= 0.5')))

plot_ridge = autoplot(fit_ridge, xvar = "lambda") +
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle("Ridge")

grid.arrange(plot_lasso, plot_el)
grid.arrange(plot_lasso, plot_ridge)



