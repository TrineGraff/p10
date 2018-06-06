##LARS
source("data_unrate.R")
source("package.R")
source("shrinkage_metoder/lars/getmin.R")
library(data.table)
set.seed(1)

lars_cv = cv.lars(x_train, y_train, type = "lar", intercept = FALSE, 
                  normalize = FALSE, trace = FALSE)
lars_fit = lars(x_train, y_train, type = "lar", intercept = FALSE, 
                normalize = FALSE)
getmin_lars = getmin_l(lars_cv$index, lars_cv$cv, lars_cv$cv.error)

coef_lars = coef(lars_fit, s = getmin_lars$lambda.1se, mode = "step")
idx_lars = which(coef_lars != 0)
length(idx_lars)

coef = data.table("LARS (CV)" = coef_lars)
coef[, feature := colnames(x_train)]

koef <- coef[feature == "DPCERA3M086SBEA" | feature == "INDPRO" |feature == "IPDMAT" 
             |feature == "CUMFNS" | feature == "HWIURATIO" | feature == "CLF16OV"
             | feature == "CE16OV" | feature == "UEMPLT5" | feature == "UEMP5TO14" 
             | feature == "UEMP15OV" | feature == "CLAIMSx" | feature == "PAYEMS" 
             | feature == "USGOOD"  | feature == "MANEMP" |feature == "TB6MS" 
             |feature == "GS1" |feature == "GS5" |feature == "EXUSUKx" 
             | feature == "lag1" ]

to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
colplot = c("blue3", "blue3", "blue3", "chartreuse4", "red3", "orange", "orange",
            "orange", "blue3", "chartreuse4", "chartreuse4","blue3", "blue3", 
            "blue3", "orange", "blue3", "blue3", "blue3", "blue3")

lars = ggplot(to_plot, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.23, 0.23) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = colplot))

