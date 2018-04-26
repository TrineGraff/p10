library(tidyverse)
library(glmnet)
source("../data/setup_data.R")
source("script/loss.R")

drops = c("UNRATE")
x = scale(data[ , !(colnames(data) %in% drops)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 


forecast_glmnet = function(y, x, idx = idx, lambda, alpha, penalty.factor = NULL, h) {
  fc = c(NA)
  n = length(y[-c(1:idx)]) - h + 1
  for(k in 0:n ) {
    y_res = y[1:(idx + k)] #y bliver opdateret med den sande værdi for hvert k
    x_kov = x[1:(idx + k), ] #tilføjede en ny række i hver iteration
    
    fit = glmnet(x_kov, y_res, alpha = alpha, intercept = FALSE,
                 penalty.factor, standardize = FALSE)
    beta_hat = coef(fit, s = lambda) %>% .[-c(1),]
    fc[k + 1] = x[dim(x_kov)[1], ] %*% beta_hat #anvender sidste række i vores design matrice og ganger på beta_hat
  }
print(fc)
}



# plot --------------------------------------------------------------------

dato = c(as.character(data_raw$dato[idx][1]), as.character(data_raw$dato[-c(1:idx)]))

df = data.frame(date = as.Date(c(dato)),
                fc_ridge, fc_lasso, fc_el, fc_grp, y = c(y[idx], y[-c(1:idx)]))

ggplot(df, aes(x = date ))  +
  geom_line(aes(y = y, colour = "Arbejdsløshed")) +
  geom_line(aes(y = fc_ridge, colour = "Ridge")) +
  geom_line(aes(y = fc_lasso, colour = "Lasso")) +
  geom_line(aes(y = fc_el, colour = "Elastik net")) +
  geom_line(aes(y = fc_grp, colour = "Group lasso")) +
  ylab("Rate") + xlab("Dato") +
  theme(legend.title=element_blank()) +
  ggtitle("One-step-ahead forecast") 


