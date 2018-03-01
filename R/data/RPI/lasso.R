source("setup_data.R")
library(lars)

fit = glmnet(X, y, family = "gaussian", alpha = 1)
plot_glmnet(fit)

cv_fit = cv.glmnet(X, y, family = "gaussian") 
plot(cv_fit) 

lambda_min = cv_fit$lambda.min
lambda_1se = cv_fit$lambda.1se

leukemia_lambdas <- unlist(cv_fit[c("lambda.min", "lambda.1se")])

parm = function(x) {
  (sum(x != 0))
}

data.frame(
  lambda = c("min", "1se"), 
  error = with(cv_fit, c(cvm[which(lambda == cv_fit$lambda.min)], cvm[which(lambda == cv_fit$lambda.1se)])),  
  p = apply(coef(fit, s = c(cv_fit$lambda.min, cv_fit$lambda.1se)), 2, parm) 
) 
#ønsker mindst kompleksitet. Så vi vælger lambda.1se

b_hat = coef(fit, s = cv_fit$lambda.1se)
idx_hat = which(b_hat != 0) 
b_hat[idx_hat, ]


# lars --------------------------------------------------------------------

#lars med lasso laver en regression model, der er straffet med L1.norm 
#(som er summen af de absolutte koefficienter)
# Dvs vi straffer nogle af koeficients værdierne (og derved kompleksiteten af modellen)
# og vi lader dem med en lille eftekt på respons variablen blive nul 
#resultatet er alle varianter af lasso, og giver en følger af koefficienter
#og fits, starter fra 0 op til det leasq squared fit
#Funktionen producerer et fit til hver ny variable bliver inkluderet. 

fit = lars(X, y, type = "lasso", trace = TRUE)
# Var = nr. variablen er i df og Step er der hvornår de blievr inkluderet. 
#plot.lars(fit)

summary(fit)
# Df = Estimated degree of freedom
# Rss the residual sum of squared
# cp The Cp startistic 
set.seed(1)
fit_lars_cv = cv.lars(X, y, type = "lasso")
#udregner cv = kryds validerings fejl (dvs mean squared predictions fejl)
#cv.error = standard error
#kryds validering for lasso chups up the l1 norm i følge af 100 punkter

#Vi finder de, som har mindst CV fejl og middelværdien med en standard afvigelse fejl
min <- min(fit_lars_cv$cv)
l1_min = fit_lars_cv$index[which.min(fit_lars_cv$cv)]
sd <- min(fit_lars_cv$cv) + fit_lars_cv$cv.error[which.min(fit_lars_cv$cv)] #mean plus en standard afvigelse
l1_1sd <- fit_lars_cv$index[min(which(fit_lars_cv$cv < sd))]

data.frame(
  lambda = c("min", "1se"), 
  error = c(min(fit_lars_cv$cv),fit_lars_cv$cv[min(which(fit_lars_cv$cv < sd))]),
  p = c(parm(coef(fit, s = l1_min,, mode = "fraction")), parm(coef(fit, s = l1_1sd,, mode = "fraction")) ) 
  #fraction fordi vi har L1 norm af vektoren (se plot)
)



prediction <- as.vector(predict(fit, X, lambda_1se, type = "class"))
prediction$fit

mean(prediction$fit - y)^2



prediction <- as.vector(predict(fit, X, s = l1_1sd, mode = "fraction"))
