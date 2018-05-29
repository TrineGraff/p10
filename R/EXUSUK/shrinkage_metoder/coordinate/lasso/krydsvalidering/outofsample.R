source("shrinkage_metoder/coordinate/lasso/krydsvalidering/insample.R")
source("shrinkage_metoder/coordinate/forecasts.R")
source("mae_mse.R")
lasso_fc = glmnet_forecast(y, x, alpha = 1, idx = (idx), lasso_cv$lambda.1se)

mae(y_test - lasso_fc)
mse(lasso_fc - y_test)

plot(lasso_fc, type = "l", ylim = c(-0.4, 0.4))
par(new = TRUE)
plot(y_test, type = "l", ylim = c(-0.4, 0.4), col = "red")

c = tidy(lasso_fc )
write.csv(c, file = "fc_lasso_kryds") 
