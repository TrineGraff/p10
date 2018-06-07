source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/lars/lars/bic/insample.R")
library(data.table)
set.seed(1)
lars_bic$f_hat
coef_lars = coef(lars_fit, s = lars_bic$f_hat, mode = "fraction")
which(coef_lars != 0)

coef = data.table("LARS (BIC)" = coef_lars)
coef[, feature := colnames(x_train)]


koef <- coef[feature == "DPCERA3M086SBEA" | feature == "INDPRO" |feature == "IPDMAT" 
             |feature == "CUMFNS" | feature == "HWIURATIO" | feature == "CLF16OV"
             | feature == "CE16OV" | feature == "UEMPLT5" | feature == "UEMP5TO14" 
             | feature == "UEMP15OV" | feature == "CLAIMSx" | feature == "PAYEMS" 
             | feature == "USGOOD"  | feature == "MANEMP" |feature == "AMDMNOx"
             |  feature == "TB6MS" 
             |feature == "GS1" |feature == "GS5" |feature == "EXUSUKx" 
             | feature == "lag1" ]

to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
colplot = c("red3","blue3", "blue3", "blue3", "chartreuse4", "red3", "orange", "orange",
            "orange", "blue3", "chartreuse4", "chartreuse4","blue3", "blue3", 
            "blue3", "orange", "blue3", "blue3", "blue3", "blue3")

lars = ggplot(to_plot, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.25, 0.25) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches"))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = colplot))


grid.arrange(lars, lasso)
