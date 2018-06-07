source("package.R")
source("data_unrate.R")
source("shrinkage_metoder/lars/getmin.R")
source("shrinkage_metoder/lars/lasso/bic/insample.R")

coef_lasso = coef(lasso_fit, s = lasso_bic$f_hat , mode = "fraction")
which(coef_lasso != 0)

coef_lasso = data.table("lasso LARS (BIC)" = coef_lasso)
coef_lasso[, feature := colnames(x_train)]

koef_lasso <- coef_lasso[feature == "DPCERA3M086SBEA" |feature == "IPDMAT" 
            | feature == "CLF16OV"
             | feature == "CE16OV" | feature == "UEMPLT5" | feature == "UEMP5TO14" 
             | feature == "UEMP15OV" | feature == "CLAIMSx" | feature == "USCONS" 
             | feature == "USTRADE"  |feature == "AMDMNOx"
             |  feature == "TB6MS" 
            |feature == "GS5" |feature == "EXUSUKx" | feature == "CPIMEDSL"
             | feature == "lag1" | feature == "lag4" ]


to_plot_lasso <- melt(koef_lasso, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
col_plot_lasso = c("red3", "blue3", "blue3", "blue3", "cadetblue2", "red3", "orange", "orange",
                   "chartreuse4", "blue3", "blue3", "orange", "blue3", "blue3", "blue3", "blue3", "blue3" )
length(col_plot_lasso)
lasso = ggplot(to_plot_lasso, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.25, 0.25) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches"))+ 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot_lasso))


grid.arrange(lars, lasso, ncol = 2)
