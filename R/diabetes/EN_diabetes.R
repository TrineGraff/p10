library(glmnet)
library(plotmo) 
library(ggfortify)
library(gridExtra)

diabetes <- read.csv("/Users/louisenygaardchristensen/Documents/AAU/10. semester/r codes/diabetes/diabetes.csv")
head(diabetes)
# p = 11 (y = prog) og n = 442

diabetes_design <- model.matrix(prog ~ . - 1, data = diabetes)
# y = prog, og vi har fjernet intercept
diabetes_design_std <- scale(diabetes_design, center = TRUE, scale = TRUE)

diabetes_response <- diabetes$prog - mean(diabetes$prog)


fit_lasso <- glmnet(diabetes_design_std, diabetes_response, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda")

plot_lasso = autoplot(fit_lasso, xvar = "lambda") + 
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  ggtitle("Lasso")

fit_ridge <- glmnet(diabetes_design_std, diabetes_response, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(fit_ridge)

fit_EN <- glmnet(diabetes_design_std, diabetes_response, alpha = 0.2, standardize = FALSE, intercept = FALSE)
plot(fit_EN)
plot(fit_EN, xvar = "lambda")

plot_EN = autoplot(fit_EN, xvar = "lambda") +
  ylab("Koefficienter") + 
  xlab(expression(log(lambda))) +
  theme(legend.title=element_blank()) +
  #ggtitle("Elastisk net")
  ggtitle(expression(paste("Elastisk net, ", alpha, ' = 0.2')))

grid.arrange(plot_lasso, plot_EN)
