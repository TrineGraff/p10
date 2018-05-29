crime <- read.csv("/Users/louisenygaardchristensen/Desktop/p10/R/crime/crime.csv")
head(crime)
# p = 6 (y = crime.rate) og n = 50

crime_design <- model.matrix(crime.rate ~ . - 1, data = crime)
# y = crime.rate, og vi har fjernet intercept
crime_design_std <- scale(crime_design, center = TRUE, scale = TRUE)

crime_response <- crime$crime.rate - mean(crime$crime.rate)

fit_lasso <- glmnet(crime_design_std, crime_response, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda")

plot_lasso = autoplot(fit_lasso, xvar = "lambda") + 
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle("Lasso")

fit_ridge <- glmnet(crime_design_std, crime_response, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda")

plot_ridge = autoplot(fit_ridge, xvar = "lambda") +
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle("Ridge regression")

grid.arrange(plot_lasso, plot_ridge)
