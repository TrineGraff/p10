library(tidyverse)

crime <- read.csv("crime.csv")
head(crime)
set.seed(1)

# lm
crime_lm <- lm(crime.rate ~ ., data = crime) 
coef(crime_lm)

# Ridge
library(MASS)
crime_ridge <- lm.ridge(crime.rate ~ ., data = crime, lambda = seq(0, 100, 0.1))
crime.lambda <- crime_ridge$lambda[which.min(crime_ridge$GCV)]
crime.ridge <- lm.ridge(crime.rate ~ ., data = crime, lambda = crime.lambda)
coef(crime.ridge)

library(glmnet)
library(glmnetUtils)

plot(glmnet(crime.rate ~ ., data = crime, alpha = 0)) # alpha = 0 svarer til Ridge
crime_ridge_cv <- cv.glmnet(crime.rate ~ ., data = crime, alpha = 0)
plot(crime_ridge_cv)

crime_ridge <- glmnet(crime.rate ~ ., data = crime, alpha = 0, lambda = crime_ridge_cv$lambda.min)
coef(crime_ridge)

# LASSO
plot(glmnet(crime.rate ~ ., data = crime, alpha = 1)) # alpha = 1 svarer til lasso
crime_lasso_cv <- cv.glmnet(crime.rate ~ ., data = crime, alpha = 1)
plot(crime_lasso_cv)

crime_lasso <- glmnet(crime.rate ~ ., data = crime, alpha = 1, lambda = unlist(crime_lasso_cv[c("lambda.min", "lambda.1se")]))
lasso_coef <- coef(crime_lasso)

lasso_coef %>% as.matrix() %>% abs() %>% colSums()

# elastisk net
plot(glmnet(crime.rate ~ ., data = crime, alpha = 0.3))


# crime - bootstrapping ---------------------------------------------------

## vurdering af lambda
crime_path <- glmnet(crime.rate ~ ., data = crime)
plot.glmnet(crime_path)

crime_cv <- cv.glmnet(crime.rate ~ ., data = crime)
plot(crime_cv)

lambda_min <- crime_cv$lambda.min
crime_fit <- glmnet(crime.rate ~ ., data = crime, lambda = lambda_min)

## Bootstrap - fixed lambda

n_bootstrap <- 100

boot_samp <- function(data) data[sample(nrow(data), size = nrow(data), replace = TRUE), ]

coef_boot_fixed_ <- replicate(n_bootstrap, coef(glmnet(crime.rate ~ ., data = boot_samp(crime), lambda = lambda_min)))
coef_boot_fixed <- sapply(coef_boot_fixed_, as.matrix) %>% t() %>% as.data.frame() %>% setNames(c("Intercept", names(crime %>% select(-crime.rate))))

boxplot(coef_boot_fixed %>% select(-Intercept))

# Bootstrap standard deviations
coef_boot_fixed %>% select(-Intercept) %>% apply(2, sd)

# Bootstrap confidence intervals
coef_boot_fixed %>% select(-Intercept) %>% lapply(quantile, prob = c(0.025, 0.975)) %>% do.call("rbind", .)

# probability of zero
p_zero_fixed <- colSums(coef_boot_fixed == 0) / n_bootstrap
p_zero_fixed

## Bootstrap - vurdere lambda for hver sample
coef_boot_lambda_ <- replicate(n_bootstrap, {
  lambda_ <- cv.glmnet(crime.rate ~ ., data = boot_samp(crime))$lambda.min
  coef(glmnet(crime.rate ~ ., data = boot_samp(crime), lambda = lambda_))
})

coef_boot_lambda <- sapply(coef_boot_lambda_, as.matrix) %>%  t() %>% as.data.frame() %>% setNames(c("Intercept", names(crime %>% select(-crime.rate))))

boxplot(coef_boot_lambda %>% select(-Intercept))

# Bootstrap standard deviations
coef_boot_lambda %>% select(-Intercept) %>% apply(2, sd)

# Bootstrap confidence intervals
coef_boot_lambda %>% select(-Intercept) %>% lapply(quantile, prob = c(0.025, 0.975)) %>% do.call("rbind", .)

# probability of zero
p_zero_lambda <- colSums(coef_boot_lambda == 0) / n_bootstrap
p_zero_lambda

