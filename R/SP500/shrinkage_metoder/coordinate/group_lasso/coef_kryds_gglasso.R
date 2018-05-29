source("data_unrate.R")
source("package.R")
library(data.table)
set.seed(1)

grp <- c(1, 1, rep(4, 3), rep(1, 14), rep(2, 27), rep(3, 10), rep(4, 4),
         rep(5, 10), rep(8, 4), rep(6, 21), rep(7, 20), rep(2, 3), rep(5, 4), rep(2, 4)) 

gglasso_cv <- cv.gglasso(x_train, y_train, group = grp, nfold = 10, intercept = FALSE, loss = "ls" )
gglasso_fit = gglasso(x_train, y_train, group = grp, intercept = FALSE, loss = "ls")
coef_gglasso = as.vector(coef(gglasso_fit, s = gglasso_cv$lambda.1se)) %>% .[-1]
# plot --------------------------------------------------------------------
coef = data.table("Group lasso" = coef_gglasso)
coef[, feature := colnames(x_train)]

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

koef_5 <- coef[ feature == "REALLN" | feature == "NONREVSL" | feature == "CONSPI" | feature == "MZMSL"
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


# plots -------------------------------------------------------------------
## ridge

gg1 <- ggplot(to_plot_1, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 1") +  
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = "chartreuse4" ))


gg2 <- ggplot(to_plot_2, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + 
  guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 2") +
  theme(axis.text.y = element_text(hjust = 1, colour = "blue3" ))


gg3 <- ggplot(to_plot_3, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 3") +
  theme(plot.margin=unit(c(0,0,1.6,0),"inches")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "purple" ))

gg4 <- ggplot(to_plot_4, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 4") +
  theme(plot.margin=unit(c(0,0,2,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = "red3" ))

gg5 <- ggplot(to_plot_5, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 5") +
  theme(plot.margin=unit(c(0,0,1.1,0),"inches")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "deeppink" ))



gg6 <- ggplot(to_plot_6, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 6") +  
  theme(axis.text.y = element_text(hjust = 1, colour = "orange" ))



gg7 <- ggplot(to_plot_7, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 7") +
  theme(axis.text.y = element_text(hjust = 1, colour = "cadetblue2" ))


gg8 <- ggplot(to_plot_8, aes(x = feature, y = coefficient)) + coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) + guides(fill = FALSE) +
  ylim(-0.21, 0.21) +
  #ylim(-0.1, 0.1) +
  ylab(" ") +
  xlab(" ") +
  ggtitle("Gruppe 8") +  
  theme(plot.margin=unit(c(0,0,6.5,0),"cm")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "goldenrod4" ))


p1 <- ggplotGrob(gg1)
p2 <- ggplotGrob(gg2)
p3 = ggplotGrob(gg3)
p4 = ggplotGrob(gg4)
p5 <- ggplotGrob(gg5)
p6 <- ggplotGrob(gg6)
p7 = ggplotGrob(gg7)
p8 = ggplotGrob(gg8)

#gemmes i 10 x 20 landscape 
grid.draw(cbind(p1, p2, p3, p4,p5, p6, p7, p8, size = "first"))


