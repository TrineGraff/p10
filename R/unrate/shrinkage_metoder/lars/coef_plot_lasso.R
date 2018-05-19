source("data_unrate.R")
source("package.R")
source("shrinkage_metoder/lars/getmin.R")
library(data.table)
set.seed(1)

#lasso
lasso_cv = cv.lars(x_train, y_train, type = "lasso", intercept = FALSE, 
                   normalize = FALSE, trace = FALSE)
lasso_fit = lars(x_train, y_train, type = "lasso", normalize = FALSE, intercept = FALSE)
getmin_lasso = getmin_l(lasso_cv$index, lasso_cv$cv, lasso_cv$cv.error)

coef_lasso = coef(lasso_fit, s = getmin$lambda.1se, mode = "fraction")
idx_lasso = which(coef_lasso != 0)
length(idx_lasso)

## plot 
coef = data.table("Lasso" = coef_lasso)
coef[, feature := colnames(x_train)]

koef <- coef[feature == "IPDMAT" | feature == "HWIURATIO" 
             | feature == "CLF16OV"| feature == "CE16OV" | feature == "UEMPLT5" 
             | feature == "UEMP5TO14" | feature == "UEMP15OV" | feature == "PAYEMS" 
             | feature == "USGOOD" 
             | feature == "USCONS" |feature == "TB6MS" | feature == "GS5" 
             | feature == "lag1" ]

to_plot <- melt(koef, id.vars = "feature", variable.names = "variable", value.name = "coefficient")
col_plot = c("blue3", "blue3", "orange", "blue3", "chartreuse4", "blue3", "blue3",
             "orange", "blue3", "blue3", "blue3", "blue3", "blue3" )


ggplot(to_plot, aes(x = feature, y = coefficient)) + 
  coord_flip() +
  geom_bar(stat = 'identity') +
  facet_wrap(~ variable, nrow = 1) +
  guides(fill = FALSE) +
  ylim(-0.23, 0.23) +
  ylab(" ") +
  xlab(" ") +
  theme(plot.margin=unit(c(0,0,0.3,0),"inches")) + 
  theme(axis.text.y = element_text(hjust = 1, colour = col_plot))

