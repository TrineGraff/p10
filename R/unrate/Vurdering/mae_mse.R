source("package.R")
source("data_unrate.R")

ar4 = read.csv(file = "results/fc_ar4.csv") 

faktor_ic1 = read.csv(file = "results/fc_faktor_ic1.csv")
faktor_ic2 = read.csv(file = "results/fc_faktor_ic2.csv")

lasso_kryds = read.csv(file = "results/fc_lasso_kryds.csv")
lasso_bic = read.csv(file = "results/fc_lasso_bic.csv")

ridge_kryds = read.csv(file = "results/fc_ridge_kryds.csv")
ridge_bic = read.csv(file = "results/fc_ridge_bic.csv")

gglasso_bic = read.csv(file = "results/fc_gglasso_bic.csv")
gglasso_kryds = read.csv(file = "results/fc_gglasso_kryds.csv")

adap_ols_kryds = read.csv(file = "results/fc_lasso_ols_kryds.csv")
adap_ols_bic = read.csv(file = "results/fc_lasso_ols_bic.csv")

adap_lasso_kryds = read.csv(file = "results/fc_adap_lasso_kryds.csv")
adap_lasso_bic = read.csv(file = "results/fc_adap_lasso_bic.csv")

lasso_TG_kryds = read.csv(file = "results/fc_lasso_TG_kryds.csv")
lasso_TG_bic = read.csv(file = "results/fc_lasso_TG_bic.csv")

lars_kryds = read.csv(file = "results/fc_lars_kryds.csv")
lars_bic = read.csv(file = "results/fc_lars_bic.csv")

lars_lasso_kryds = read.csv(file = "results/fc_lars_lasso_kryds.csv")
lars_lasso_bic = read.csv(file = "results/fc_lars_lasso_bic.csv")

lars_TG_kryds = read.csv(file = "results/fc_lars_TG_kryds.csv")
lars_TG_bic = read.csv(file = "results/fc_lars_TG_bic.csv")


diffs <- data.frame(diffs <- data.frame( ic2 = faktor_ic2$x  - y_test, 
                                         lasso_TG_kryds = lasso_TG_kryds[,2] - y_test, 
                                         lasso_TG_bic = lasso_TG_bic[,2] - y_test,
                                         lars_TG_kryds = lars_TG_kryds[,2] - y_test, 
                                         lars_TG_bic =  lars_TG_bic[,2] - y_test)
)


# Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Root Mean Squared Error
mse <- function(error)
{
  (mean(error^2))
}

results <- list(
  mae =round(mae(diffs$lars_TG_bic), digits = 4),
  relative_mae = round(mae(diffs$lars_TG_bic) / mae(diffs$ic2), digits = 4),
  mse = round(mse(diffs$lars_TG_bic), digits = 4),
  relative_mse = round(mse(diffs$lars_TG_bic) / mse(diffs$ic2), digits = 4))

mse(diffs$adap_ols_kryds...2....y.test)
mse(diffs$adap_ols_bic...2....y.test)
mse(diffs$adap_lasso_kryds...2....y.test)
mse(diffs$adap_lasso_bic...2....y.test)
