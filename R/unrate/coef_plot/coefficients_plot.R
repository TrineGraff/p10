source("../scripts/setup_data.R")
set.seed(1)

library(glmnet)
library(ggplot2)
library(gridExtra)
library(data.table)


# lasso
cv.lasso <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE)
plot(cv.lasso)
best_lambda <- cv.lasso$lambda.1se
lasso_coef <- cv.lasso$glmnet.fit$beta[, cv.lasso$glmnet.fit$lambda == best_lambda]
lasso_coef[lasso_coef != 0]

## ridge regression
cv.ridge <- cv.glmnet(X.train, y.train, alpha = 0, standardize = FALSE, intercept = FALSE)
plot(cv.ridge)
best_lambda <- cv.ridge$lambda.min
ridge_coef <- cv.ridge$glmnet.fit$beta[, cv.ridge$glmnet.fit$lambda == best_lambda]

## elastisk net
cv.EN <- cv.glmnet(X.train, y.train, alpha = 0.89, standardize = FALSE, intercept = FALSE)
plot(cv.EN)
best_lambda <- cv.EN$lambda.1se
EN_coef <- cv.EN$glmnet.fit$beta[, cv.EN$glmnet.fit$lambda == best_lambda]

## group lasso
library(gglasso)
grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2,4)) 
# lag 1 - lag 4 tilhører gruppe 2 som unrate
cv.group_lasso <- cv.gglasso(X.train, y.train, group = grp, intercept = FALSE)
plot(cv.group_lasso)
best_lambda <- cv.group_lasso$lambda.1se
group_lasso_coef <- cv.group_lasso$gglasso.fit$beta[, cv.group_lasso$gglasso.fit$lambda == best_lambda] 

## adaptive lasso
# OLS vægte
df.OLS.X.train <- data.frame(X.train)
lm.model <- lm(y.train ~. -1, data = df.OLS.X.train)
OLS_coef <- coef(lm.model)
cv.alasso.OLS <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE,
                               penalty.factor = 1 / abs(OLS_coef))
plot(cv.alasso.OLS)
best_lambda <- cv.alasso.OLS$lambda.min
alasso.OLS_coef <- cv.alasso.OLS$glmnet.fit$beta[, cv.alasso.OLS$glmnet.fit$lambda == best_lambda]

# lasso vægte
cv.alasso.ridge <- cv.glmnet(X.train, y.train, alpha = 1, standardize = FALSE, intercept = FALSE, 
                                 penalty.factor = 1 / abs(ridge_coef))
plot(cv.alasso.ridge)
best_lambda <- cv.alasso.ridge$lambda.1se
alasso.ridge_coef <- cv.alasso.ridge$glmnet.fit$beta[, cv.alasso.ridge$glmnet.fit$lambda == best_lambda]
alasso.ridge_coef[alasso.ridge_coef != 0]

# plots -------------------------------------------------------------------
coef = data.table(Lasso = lasso_coef,
                  Ridge_regression = ridge_coef,
                  Elastisk_net = EN_coef,
                  Group_lasso = group_lasso_coef,
                  Adaptive_lasso_OLS = alasso.OLS_coef,
                  Adaptive_lasso_ridge = alasso.ridge_coef)
coef[, feature := names(ridge_coef)]
#coef[, group := grp]


koef_1 <- coef[feature == "RPI" | feature == "W875RX1" | feature == "INDPRO" | feature == "IPFPNSS"
               | feature == "IPFINAL" | feature == "IPCONGD" | feature == "IPDCONGD"
               | feature == "IPNCONGD" | feature == "IPBUSEQ" | feature == "IPMAT"
               | feature == "IPDMAT" | feature == "IPNMAT" | feature == "IPMANSICS" 
               | feature == "IPB51222S" | feature == "IPFUELS" | feature == "CUMFNS"]

koef_2 <- coef[feature == "HWI" | feature == "HWIURATIO" | feature == "CLF16OV" | feature == "CE16OV"
               | feature == "UEMPMEAN" | feature == "UEMPLT5" | feature == "UEMP5TO14"
               | feature == "UEMP15OV" | feature == "UEMP15T26" | feature == "UEMP27OV"
               | feature == "CLAIMSx" | feature == "PAYEMS" | feature == "USGOOD" | feature == "CES1021000001"
               | feature == "USCONS" | feature == "MANEMP" | feature == "DMANEMP"
               | feature == "NDMANEMP" | feature == "SRVPRD" | feature == "USTPU" | feature == "USWTRADE"
               | feature == "USTRADE" | feature == "USFIRE" | feature == "USGOVT" | feature == "CES0600000007"
               | feature == "AWOTMAN" | feature == "AWHMAN" | feature == "CES0600000008"
               | feature == "CES2000000008" | feature =="CES3000000008" | feature == "lag 1"
               | feature == "lag 2" | feature == "lag 3" | feature == "lag 4"]

koef_3 <- coef[feature == "HOUST" | feature == "HOUSTNE" | feature == "HOUSTMW" | feature == "HOUSTS"
               | feature == "HOUSTW" | feature == "PERMIT" | feature == "PERMITNE"
               | feature == "PERMITMW" | feature == "PERMITS" | feature == "PERMITW"]

koef_4 <- coef[feature == "DPCERA3M086SBEA" | feature == "CMRMTSPLx" | feature == "RETAILx"
               | feature == "AMDMNOx" | feature == "AMDMUOx" | feature == "BUSINVx"
               | feature == "ISRATIOx"]

koef_5 <- coef[feature == "M1SL" | feature == "M2SL" | feature == "M2REAL" | feature == "AMBSL"
               | feature == "TOTRESNS" | feature == "NONBORRES" | feature == "BUSLOANS"
               | feature == "REALLN" | feature == "NONREVSL" | feature == "CONSPI" | feature == "MZMSL"
               | feature == "DTCOLNVHFNM" | feature == "DTCTHFNM" | feature == "INVEST"]

koef_6 <- coef[feature == "FEDFUNDS" | feature == "CP3Mx" | feature == "TB3MS" | feature == "TB6MS"
               | feature == "GS1" | feature == "GS5" | feature == "GS10" | feature == "AAA"
               | feature == "BAA" | feature == "COMPAPFFx" | feature == "TB3SMFFM" | feature == "TB6SMFFM"
               | feature == "T1YFFM" | feature == "T5YFFM" | feature == "T10YFFM" | feature == "AAAFFM"
               | feature == "BAAFFM" | feature == "EXSZUSx" | feature == "EXJPUSx"
               | feature == "EXUSUKx" | feature == "EXCAUSx"]

koef_7 <- coef[feature == "WPSFD49207" | feature == "WPSFD49502" | feature == "WPSID61" | feature == "WPSID62"
               | feature == "OILPRICEx" | feature == "PPICMM" | feature == "CPIAUCSL" | feature == "CPIAPPSL"
               | feature == "CPITRNSL" | feature == "CPIMEDSL" | feature == "CUSR0000SAC" | feature == "CUSR0000SAD"
               | feature == "CUSR0000SAS" | feature == "CPIULFSL" | feature == "CUSR0000SA0L2" | feature == "CUSR0000SA0L5"
               | feature == "PCEPI" | feature == "DDURRG3M086SBEA" | feature == "DNDGRG3M086SBEA" | feature == "DSERRG3M086SBEA"]

koef_8 <- coef[feature == "S.P.500" | feature == "S.P..indust" | feature == "S.P.div.yield"
               | feature == "S.P.PE.ratio"]

to_plot_1 <- melt(koef_1, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_2 <- melt(koef_2, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_3 <- melt(koef_3, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_4 <- melt(koef_4, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_5 <- melt(koef_5, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_6 <- melt(koef_6, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_7 <- melt(koef_7, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
to_plot_8 <- melt(koef_8, id.vars = "feature", variable.names = "variable", value.name = "coefficient")


gg1 <- ggplot(to_plot_1, aes(x = feature, y = coefficient, fill = variable)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 1") +  
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) 

gg2 <- ggplot(to_plot_2, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 2") 

gg3 <- ggplot(to_plot_3, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 3") +
  theme(plot.margin=unit(c(0,0,1.6,0),"inches")) 

gg4 <- ggplot(to_plot_4, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 4") +
  theme(plot.margin=unit(c(0,0,2,0),"inches")) 

gg5 <- ggplot(to_plot_5, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  #ylim(-0.27, 0.27) +
  #ylim(-0.1, 0.1) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 5") +
  theme(plot.margin=unit(c(0,0,1.1,0),"inches")) 

gg6 <- ggplot(to_plot_6, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 6") 

gg7 <- ggplot(to_plot_7, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 7") 

gg8 <- ggplot(to_plot_8, aes(x = feature, y = coefficient, fill = variable)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.27, 0.27) +
  #ylim(-0.1, 0.1) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 8") +  
  theme(plot.margin=unit(c(0,0,6.5,0),"cm"))
