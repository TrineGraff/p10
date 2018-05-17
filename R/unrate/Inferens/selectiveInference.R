source("../scripts/setup_data.R")

library(selectiveInference)
# Forfatter: Ryan Tibshirani, Rob Tibshirani, Jonathan Taylor,
# Joshua Loftus, Stephen Reid
# main fcts: lar, larInf, fixedLassoInf
set.seed(1)


# lar og larInf -----------------------------------------------------------

# kun for lar ikke lars.lasso!!!
larfit <- lar(X.train, y.train, intercept = FALSE, normalize = FALSE, verbose = TRUE)
# output: bør give samme output som lars pakken, men returnerer også en ekstra information 
# (polyhedral betingelserne) som skal bruges for selective inferens udregninger.
# Values of lambda (knots) visited along the path - går fra lambda=63.049 til lambda=0.007
plot(larfit)
#coef(larfit, s = ?)

# udregner sekventielle p-værdier og konfidensintervaller 
# (sigma er estimeret fra den fulde model)
out = larInf(larfit, alpha = 0.1)
# Resultatet printes for hver variabel når den enter modellen (kan ændres med type argument)



# p-værdier for de aktive variable
out$pv
# Konfidensintervaller
out$ci
# vektor af outcome
out$y
# p-værdier for spacing testen (M+ anvendes)
out$pv.spacing
# p-værdier for modificeret spacing test (M+ erstattes med næste knot)
out$pv.modspac
# p-værdier for kovarians testen
out$pv.covtest






# fixedLassoInf -------------------------------------------------------------------

# fixedLassoInf: Lasso inferens funktion for fast lambda.
# Bemærk vi giver inferens for løsningen af 
# min 1/2 ||y - beta_0 - X beta||_2^2 + lambda ||beta||_1
# mens glmnet løser:
# min 1/2n ||y - beta_0 - X beta||_2^2 + lambda ||beta||_1

lasso.model <- glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
best_lambda <- cv.lasso$lambda.1se

n <- length(y.train)
lambda <- best_lambda * n
beta = coef(lasso.model, s = lambda/n, exact = TRUE, x = X.train, y = y.train)[-1]

out_glm <- fixedLassoInf(X.train, y.train, beta, lambda, intercept = FALSE, alpha = 0.1)
?fixedLassoInf

out_glm$type # partial
out_glm$lambda
out_glm$ci # konfidensinterval i tabel
round(out_glm$pv, digit = 3) # p-værdier i tabel
round(out_glm$tailarea, digits = 3)
out_glm$vlo # ikke i tabel
out_glm$vup
out_glm$vmat # lin kontraster der definere de observerede statistikker
out_glm$y # vektor af outcomes (blot y.train)
out_glm$vars
out_glm$sign # fortegn af de aktive variable
out_glm$sigma
out_glm$alpha
out_glm$sd
out_glm$coef0 # koefficienterne
out_glm$call

z.score.3 <- (-0.0024519746 - mean(out_glm$coef0)) / 0.001787111

z.score.ipdmat <- (-0.0026300725 - mean(out_glm$coef0))/0.002367498
z.score.ipdmat <- (-0.0026300725 - mean(out_glm$coef0))/out_glm$sigma

length(out_glm$coef0)
# pv: one-sided p-værdier for aktive variable, anvender at vi har betinget på fortegnet
# tailarea: realized tail ares (øvre og nedre) for hvert konfidensinterval
# vlo: nedre truncation grænser for statistics
# vup: øvre truncation limits for statistics


# 3, 14, 21, 22, 23, 25, 26, 27, 31, 34, 78, 80, 94, 123
X.train[, 3] == X.train[, 'DPCERA3M086SBEA']
X.train[, 14] == X.train[, 'IPDMAT']
X.train[, 21] == X.train[, 'HWIURATIO']
X.train[, 22] == X.train[, 'CLF16OV']
X.train[, 23] == X.train[, 'CE16OV']
X.train[, 25] == X.train[, 'UEMPLT5']
X.train[, 26] == X.train[, 'UEMP5TO14']
X.train[, 27] == X.train[, 'UEMP15OV']
X.train[, 31] == X.train[, 'PAYEMS']
X.train[, 34] == X.train[, 'USCONS']
X.train[, 78] == X.train[, 'TB6MS']
X.train[, 80] == X.train[, 'GS5']
X.train[, 94] == X.train[, 'EXUSUKx']
X.train[, 123] == X.train[, 'lag 1']


getAnywhere(fixedLassoInf())
