source("package.R")
source("data_unrate.R")

ar4 = read.csv(file = "results/fc_ar4.csv") %>% .$x

faktor_ic1 = read.csv(file = "results/fc_faktor_ic1.csv")%>% .$x
faktor_ic2 = read.csv(file = "results/fc_faktor_ic2.csv")%>% .$x
faktor_ic3 = read.csv(file = "results/fc_faktor_ic3.csv")%>% .$x

lasso_kryds = read.csv(file = "results/fc_lasso_kryds.csv")%>% .$x
lasso_bic = read.csv(file = "results/fc_lasso_bic.csv")%>% .$x

ridge_kryds = read.csv(file = "results/fc_ridge_kryds.csv")%>% .$x
ridge_bic = read.csv(file = "results/fc_ridge_bic.csv")%>% .$x

gglasso_bic = read.csv(file = "results/fc_gglasso_bic.csv")%>% .$x
gglasso_kryds = read.csv(file = "results/fc_gglasso_kryds.csv")%>% .$x

adap_ols_kryds = read.csv(file = "results/fc_lasso_ols_kryds.csv")%>% .$x
adap_ols_bic = read.csv(file = "results/fc_lasso_ols_bic.csv")%>% .$x

adap_lasso_kryds = read.csv(file = "results/fc_adap_lasso_kryds.csv")%>% .$x
adap_lasso_bic = read.csv(file = "results/fc_adap_lasso_bic.csv")%>% .$x

lars_kryds = read.csv(file = "results/fc_lars_kryds.csv")%>% .$x
lars_bic = read.csv(file = "results/fc_lars_bic.csv")%>% .$x

lars_lasso_kryds = read.csv(file = "results/fc_lars_lasso_kryds.csv")%>% .$x
lars_lasso_bic = read.csv(file = "results/fc_lars_lasso_bic.csv")%>% .$x


# MCS ---------------------------------------------------------------------
diffs <- data.frame(ar4 - y_test,
                    faktor_ic1  - y_test,
                    faktor_ic2  - y_test,
                    lasso_kryds - y_test,
                    lasso_bic - y_test,
                    ridge_kryds  - y_test,
                    ridge_bic  - y_test,
                    gglasso_kryds  - y_test,
                    gglasso_bic  - y_test,
                    adap_ols_kryds   - y_test,
                    adap_ols_bic   - y_test,
                    adap_lasso_kryds  - y_test,
                    adap_lasso_bic  - y_test,
                    lars_kryds   - y_test,
                    lars_bic   - y_test,
                    lars_lasso_kryds  - y_test,
                    lars_lasso_bic  - y_test )

colnames(diffs) = c("AR(4)", "Faktor_1", "Faktor_2", "Faktor_3", "lasso (kryds)", "lasso (bic)",
                     "ridge (kryds)", "ridge (bic)", "group lasso (kryds)", "group lasso (bic)",
                     "adap. lasso m. ols vægte (kryds)","adap. lasso m. ols vægte (bic)",
                     "adap. lasso m. lasso vægte (kryds)","adap. lasso m. lasso vægte (bic)",
                     "LARS (kryds)", "LARS (BIC)", "lasso m. lars modifikation (kryds)", "lasso m. lars modifikation (bic)")

# Tabs tabeller for MCS
loss <- list(
  abs = abs(diffs),
  sq = (diffs)^2
)

res <- setNames(vector("list", length(loss)), names(loss))
for (ln in names(loss)) {
  MCS <- MCSprocedure(Loss = diffs, alpha = 0.1, B = 5000, statistic = 'Tmax')
  res[[ln]] <- MCS
}

saveRDS(res, file = "mcs90.rds")


?MCSprocedure
