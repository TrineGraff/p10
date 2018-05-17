source("../scripts/setup_data.R")
set.seed(1)
library(xtable)
library(ggplot2)

library(covTest) # udregner kovarianstest for adaptive lineær modellering
# Forfatter: Rob Tibshirani
# Referencer: A significance test for the lasso (2013). Lockhart, R., Taylor, J., 
# Tibshirani (Ryan) and Tibshirani (Robert)

# covTest(fitobj, x, y, sigma.est = "full", status = NULL, maxp=min(nrow(x),ncol(x)))
# fitobj: Result of a call to lars or lars.en or lars.glm
# sigma.est: Estimate of error standard deviation. If a numerical value, that value if used. 
# If "full" the (square root) of the mean squared residual from the full model is used.

# lars.en(x, y, lambda2,normalize=TRUE)
# lars.glm: family = c("binomial", "cox"), derfor skal den ikke bruges

library(lars)
lars.lasso <- lars(X.train, y.train, type = "lasso", trace = TRUE, 
                   normalize = FALSE, intercept = FALSE) 
# 192 steps (variablerne tilføjes og nogle fjernes igen)
summary.lars.lasso <- summary(lars.lasso)

# Find modellen udfra krydsvaliering
cv.lasso <- cv.lars(X.train, y.train, type = "lasso", plot.it = TRUE, se = TRUE, 
                    normalize = FALSE, intercept = FALSE, mode = "fraction")

limit <- min(cv.lasso$cv) + cv.lasso$cv.error[which.min(cv.lasso$cv)]
s.cv <- cv.lasso$index[min(which(cv.lasso$cv < limit))]
lasso_coef_cv <- coef(lars.lasso, s = s.cv, mode = "fraction")
lasso_coef_cv[lasso_coef_cv!=0] # vælger 13 prædiktorer

# Find modellen udfra Cp 
s.Cp = which.min(summary.lars.lasso$Cp)
lasso_coef_cp <- coef(lars.lasso, s = s.Cp, mode="step")
lasso_coef_cp[lasso_coef_cp!=0] # vælger 21 prædiktorer

# Cp is better than CV for smaller sample size


# CovTest -----------------------------------------------------------------

simple_regression <- lm(y.train ~ 0 + X.train)
sigma.est = summary(simple_regression)$sig # sigma.hat = 0.04329215

result <- covTest(lars.lasso, X.train, y.train)
# sigma.hat = 0.0433, nul-ford: F(2,422)
?covTest

xtable(result$results, type = "latex")

# 27, 25, 23 og 22 
X.train[, 22] == X.train[, 'CLF16OV']
X.train[, 23] == X.train[, 'CE16OV']
X.train[, 25] == X.train[, 'UEMPLT5']
X.train[, 27] == X.train[, 'UEMP15OV']

test_statistics_NA <- result$results[,2]
test_statistics <- test_statistics_NA[!is.na(test_statistics_NA)]

qqexp.plot = function(test_statistics){
  q.sample_l = quantile(test_statistics)[["25%"]]
  q.sample_u = quantile(test_statistics)[["75%"]]
  q.theory_l = qexp(0.25)
  q.theory_u = qexp(0.75)
  slope = (q.sample_l - q.sample_u)/(q.theory_l-q.theory_u)
  int = q.sample_l - slope*q.theory_l
  ggplot() +
    stat_qq(aes(sample = test_statistics)) +
    geom_abline(intercept = int, slope = slope) +
    xlab("Teoretisk") +
    ylab("Empirisk") +
    ggtitle("Exp Q-Q plot") 
}
qqexp.plot(test_statistics)

?qnorm
?qexp

lars.en <- lars.en(X.train, y.train, lambda2 = 0.1, normalize = FALSE)
covTest(lars.en, X.train, y.train)


# EN ----------------------------------------------------------------------

# Elastic Net: Tj is simply scaled by (1 + λ2), where λ2 multiplies the l2 penalty
