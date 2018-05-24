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

mean(abs(diffs$ar4...2....y.test))
cummean(abs(diffs$ar4...2....y.test))

MSE_rol  = function(error) {
  (cummean(abs(error)))
}

AR4 <- MSE_rol(diffs$ar4...2....y.test)
Faktor1 <- MSE_rol(diffs$faktor_ic1...2....y.test)
Bench <- MSE_rol(diffs$faktor_ic2...2....y.test)
Faktor3 <- MSE_rol(diffs$faktor_ic3...2....y.test)
Lasso_CV <-MSE_rol(diffs$lasso_kryds...2....y.test)
Lasso_BIC <- MSE_rol(diffs$lasso_bic...2....y.test)
Ridge_CV <- MSE_rol(diffs$ridge_kryds...2....y.test)
Ridge_BIC <- MSE_rol(diffs$ridge_bic...2....y.test)
gglasso_CV <- MSE_rol(diffs$gglasso_kryds...2....y.test)
gglasso_BIC <- MSE_rol(diffs$gglasso_bic...2....y.test)
Alasso.ols_CV <- MSE_rol(diffs$adap_ols_kryds...2....y.test)
Alasso.ols_BIC <- MSE_rol(diffs$adap_ols_bic...2....y.test)
Alasso.lasso_CV <- MSE_rol(diffs$adap_lasso_kryds...2....y.test)
Alasso.lasso_BIC <- MSE_rol(diffs$adap_lasso_bic...2....y.test)
LARS_CV <- MSE_rol(diffs$lars_kryds...2....y.test)
LARS_BIC <- MSE_rol(diffs$lars_bic...2....y.test)
LARS.lasso_CV <- MSE_rol(diffs$lars_lasso_kryds...2....y.test)
LARS.lasso_BIC <- MSE_rol(diffs$lars_lasso_bic...2....y.test)


dato <- c(as.character(dato.test[-c(1:4)]))
df <- data.frame(dato = as.Date(dato), Bench = Bench / Bench, AR4 = AR4 / Bench, Faktor1 = Faktor1 / Bench, 
                 Faktor3 = Faktor3 / Bench, Lasso_CV = Lasso_CV / Bench, Lasso_BIC = Lasso_BIC / Bench, 
                 Ridge_CV = Ridge_CV / Bench, Ridge_BIC = Ridge_BIC / Bench, gglasso_CV = gglasso_CV / Bench, 
                 gglasso_BIC = gglasso_BIC / Bench, Alasso.ols_CV = Alasso.ols_CV / Bench, 
                 Alasso.ols_BIC = Alasso.ols_BIC / Bench, Alasso.lasso_CV = Alasso.lasso_CV / Bench, 
                 Alasso.lasso_BIC = Alasso.lasso_BIC / Bench, LARS_CV = LARS_CV / Bench, 
                 LARS_BIC = LARS_BIC / Bench, LARS.lasso_CV = LARS.lasso_CV / Bench, LARS.lasso_BIC = LARS.lasso_BIC / Bench)

df_test = data.frame(dato = as.Date(dato), Bench = Bench / Bench, AR4 = AR4 / Bench)

ggplot(df, aes(x = dato)) +
  geom_line(aes(y = Bench, color = "Benchmark")) +
  geom_line(aes(y = AR4, colour = "AR4")) +
  geom_line(aes(y = Faktor1, colour = "Faktor model (IC1)")) + 
  geom_line(aes(y = Faktor3, colour = "Faktor model (IC3)")) + 
  geom_line(aes(y = Lasso_CV, colour = "Lasso (CV)")) +
  geom_line(aes(y = Lasso_BIC, colour = "Lasso (BIC)")) +
  geom_line(aes(y = Ridge_CV, colour = "Ridge regression (CV)")) +
  geom_line(aes(y = Ridge_BIC, colour = "Ridge regression (BIC)")) +
  geom_line(aes(y = gglasso_CV, colour = "Group lasso (CV)")) +
  geom_line(aes(y = gglasso_BIC, colour = "Group lasso (BIC)")) +
  geom_line(aes(y = gglasso_CV, colour = "Group lasso (CV)")) +
  geom_line(aes(y = Alasso.ols_CV, colour = "Adaptive lasso m. OLS (CV)")) +
  geom_line(aes(y = Alasso.ols_BIC, colour = "Adaptive lasso m. OLS (BIC)")) +
  geom_line(aes(y = Alasso.lasso_CV, colour = "Adaptive lasso m. lasso (CV)")) +
  geom_line(aes(y = Alasso.lasso_BIC, colour = "Adaptive lasso m. OLS (BIC)")) +
  geom_line(aes(y = LARS_CV, colour = "LARS u. lasso modifikation (CV)")) +
  geom_line(aes(y = LARS_BIC, colour = "LARS u. lasso modifikation (BIC)")) +
  geom_line(aes(y = LARS.lasso_CV, colour = "LARS m. lasso modifikation (CV)")) +
  geom_line(aes(y = LARS.lasso_BIC, colour = "LARS m. lasso modifikation (BIC)")) +
  #ylim(0.2,1.4) +
  xlab(" ") +
  ylab("Ratio") + labs(color='Models') 
