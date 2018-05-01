library(lars)
library(covTest)
library(selectiveInference)

crime <- read.csv("crime.csv")
head(crime)
set.seed(1)

y <- scale(crime[, 6], center = TRUE, scale = FALSE) # crimerate
X <- scale(as.matrix(crime[, 1:5]), center = TRUE, scale = TRUE)

# lasso
fit <- glmnet(X, y, alpha = 1)
cvfit <- cv.glmnet(X, y, alpha = 1)
plot(cvfit)

# Plottet indkluderer kryds-validerings kurven (røde stiplede linje) og øvre og nedre standard
# afvigelses kurve langs x-aksen (error bars). 
# To valgte λ’er er indikeret af lodrette stiplede linjer
(lambda_min <- cvfit$lambda.min)
# lambda.min er værdien af λ som giver den minste mean kryds-valierings fejl (misclassification error)
(lambda_1se <- cvfit$lambda.1se)
# lambda.1se giver den mest regulerede model (største værdi af lambda) s.a. fejlen er indenfor en standard fejl af minimum 

data.frame(
  name = c("min", "1se"),
  lambda = c(cvfit$lambda.min, cvfit$lambda.1se),
  error = c(cvfit$cvm[which(cvfit$lambda == lambda_min)], 
            cvfit$cvm[which(cvfit$lambda == lambda_1se)]),
  p = c(sum(coef(fit, s = lambda_min) != 0), 
        sum(coef(fit, s = lambda_1se) != 0))
)
# lambda_min = 15.16, med en fejl på 70850.9 og p = 4
# lambda_1se = 155.15, med en fejl på 90837.6 og p = 1

# Vi vælger lambda_1se som værdien af lambda for den resterende del af analysen

# For de indkluderede parametre har vi følgende estimater
beta_hat <- coef(fit, s = lambda_1se)
idx_hat <- which(beta_hat != 0)
beta_hat[idx_hat, ]

# Klassifikation fejlen (krydsvaliderings fejlen) for insample prediction med disse parametre er
prediction <- as.vector(predict(fit, X, lambda_1se, type = "class"))
misclassified = (prediction != y)
mean(misclassified) # 1??? (for begge lambda'er)

# Vha bootstrap metoden, vil vi quantificere variation af beta_hat.
# Vi resampler datasættet 1000 gange og udtrækker koefficienterne for hver realization
# Vi slicer bootstrap data til kun at indeholde de valgte variable

# Bootstrapping 
n_bootstrap <- 1000
crime_n <- nrow(crime)
bootstrap_idx <- replicate(n_bootstrap, sample(crime_n, size = crime_n, replace = TRUE), simplify = FALSE)

crime_boot <- sapply(bootstrap_idx, function(idx) {
  as.matrix(coef(glmnet(X[idx,], y[idx], lambda = lambda_1se)))
})

crime_boot_hat <- t(crime_boot[idx_hat, ]) %>% 
  as_tibble() %>% 
  setNames(rownames(beta_hat)[idx_hat])

boxplot(crime_boot_hat, horizontal = TRUE, las = 1)
abline(v=0, lty = 1)


# covTest -----------------------------------------------------------------


object <- lars(X, y, type = "lasso")
plot(object)
fits <- predict.lars(object, X, type = "fit")
coef4.1 <- coef(object, s = 4.1, mode = "norm")
coef4.1 <- predict(object, s = 4.1, type = "coef", mode = "norm")


obejct_Test <- covTest(object, x = X, y = y)
