# `glmnet`
library(tidyverse)
library(glmnet)

crime <- read_csv("crime.csv", col_types = cols()) %>% 
  rename(rate = `crime rate`, not_hs = `not-hs`) %>% as.data.frame()
head(crime)

# `glmnet` expects vector response `y` and a design matrix `x`. Hence, for general use, 
# one needs to construct a design matrix for the regression model. 
# If no factor variables, this is equivalent to: as.matrix(data[,!grepl("y", names(data))])
as.matrix(crime[,!grepl("rate", names(crime))])

crime_design <- model.matrix(rate ~ . + 0, data = crime) ## + 0 or - 1 to exclude intercept (fitted by glmnet)
crime_lasso <- glmnet(x = crime_design, y = crime$rate, alpha = 1) ## LASSO
plot(crime_lasso)

# The library `plotmo` and `ggfortify` facilitate easier plotting of the `glmnet` ourput.

library(plotmo)
plot_glmnet(crime_lasso)

library(ggfortify)
autoplot(crime_lasso)

# `glmnetUtils`

# The package `glmnetUtils` provides a formula interface to the model 
# specification - i.e. we can adapt the notation of `response ~ predictors, 
# data = data_object` in the `glmnet` call. Hence, we aviod the manual
# specification of the `model.matrix` and extraction of the response (cf above). 
# This is true for both the fit and the cross-validation functions.

library(glmnetUtils)

crime_lasso_ <- glmnet(rate ~ ., data = crime, alpha = 1) ## LASSO
autoplot(crime_lasso_)

crime_lasso_cv <- cv.glmnet(rate ~ ., data = crime, alpha = 1) ## LASSO
autoplot(crime_lasso_cv)

# We see that the `lambda.min` and `lambda.1se` are given as 
(crime_lambdas <- unlist(crime_lasso_cv[c("lambda.min", "lambda.1se")]))

coef(crime_lasso, s = crime_lambdas)

crime_coefs <- coef(crime_lasso, s = crime_lambdas)
crime_coefs %>% as.matrix() %>% abs() %>% colSums()

crime_coefs_nonzero <- predict(crime_lasso, s = crime_lambdas, type = "nonzero")

# `grplasso`

library(grplasso)

lambda_max <- lambdamax(rate ~ ., data = crime, model = LinReg())
lambda_seq <- seq(from = lambda_max, to = 0.5, len = 100)

crime_grplasso <- grplasso(rate ~ ., data = crime, model = LinReg(), lambda = lambda_seq, 
                           control = grpl.control(trace = 0))



crime$funding_factor <- cut(crime$funding, quantile(crime$funding, c(0, 0.33, 0.66, 1)), include.lowest = TRUE)

crime_grplasso_factor <- suppressWarnings(
  grplasso(rate ~ . - funding, data = crime, model = LinReg(), lambda = lambda_seq, 
           control = grpl.control(trace = 0))
)

# We see that either both levels of `funding_factor` are zero or both non-zero. 
# For comparison, we see that ordinary `lasso` do not obey this

crime_design_factor <- model.matrix(rate ~ . - funding, data = crime)
crime_lasso_factor <- 
  grplasso(y = crime$rate, x = crime_design_factor, index = c(NA,1:(ncol(crime_design_factor)-1)),
           model = LinReg(), lambda = lambda_seq, control = grpl.control(trace = 0))

crime_lasso_coef_factor <- coef(crime_lasso_factor)
crime_lasso_funding <- crime_lasso_coef_factor[grepl("funding_", rownames(crime_lasso_coef_factor)),] %>% 
  t() %>% as.data.frame() %>% mutate(lambda = as.numeric(rownames(.)))

crime_grplasso_coef_factor <- coef(crime_grplasso_factor)
crime_grplasso_funding <- crime_grplasso_coef_factor[grepl("funding_", rownames(crime_grplasso_coef_factor)),] %>% 
  t() %>% as.data.frame() %>% mutate(lambda = as.numeric(rownames(.)))

bind_rows(list(grplasso = crime_grplasso_funding, lasso = crime_lasso_funding), .id = "method") %>% 
  gather(key = "variable", value = "estimate", starts_with("funding")) %>% 
  ggplot(aes(x = lambda, y = estimate, colour = variable, linetype = method)) + geom_path() +
  scale_x_log10() + theme(legend.position = "top")

