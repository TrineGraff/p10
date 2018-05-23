library(MCS)
source("../scripts/setup_data.R")

ar4 = read.csv(file = "results/fc_ar4.csv") 

faktor_ic1 = read.csv(file = "results/fc_faktor_ic1.csv")
faktor_ic2 = read.csv(file = "results/fc_faktor_ic2.csv")
faktor_ic3 = read.csv(file = "results/fc_faktor_ic3.csv")

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

lars_kryds = read.csv(file = "results/fc_lars_kryds.csv")
lars_bic = read.csv(file = "results/fc_lars_bic.csv")

lars_lasso_kryds = read.csv(file = "results/fc_lars_lasso_kryds.csv")
lars_lasso_bic = read.csv(file = "results/fc_lars_lasso_bic.csv")

diffs <- data.frame(ar4[,2] - y.test, faktor_ic1[,2] - y.test, faktor_ic2[,2] - y.test, 
                    faktor_ic3[,2] - y.test, lasso_kryds[,2] - y.test, lasso_bic[,2] - y.test, 
                    ridge_kryds[,2] - y.test, ridge_bic[,2] - y.test,
                    gglasso_kryds[,2] - y.test, gglasso_bic[,2] - y.test, 
                    adap_ols_kryds[,2] - y.test, adap_ols_bic[,2] - y.test,
                    adap_lasso_kryds[,2] - y.test, adap_lasso_bic[,2] - y.test, 
                    lars_kryds[,2] - y.test, lars_bic[,2] - y.test,
                    lars_lasso_kryds[,2] - y.test, lars_lasso_bic[,2] - y.test)

# Tabs tabeller for MCS
loss <- list(
  abs = abs(diffs),
  sq = (diffs)^2
)

?MCSprocedure

res <- setNames(vector("list", length(loss)), names(loss))
for (ln in names(loss)) {
  MCS <- MCSprocedure(Loss = diffs, alpha = 0.1, B = 5000, statistic = 'Tmax')
  res[[ln]] <- MCS
}

saveRDS(res, file = "mcs90_Tmax.rds")
