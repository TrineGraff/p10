source("script/script_forecast_glmnet.R")
source("script/script_forecast_gglasso.R")

# ridge -------------------------------------------------------------------
lambda_ridge = read.csv("results/ridge_lambda.csv") %>% .[1, 2]
fc_ridge = forecast_glmnet(y, x, idx = idx, lambda = lambda_ridge, alpha = 0)

# lasso -------------------------------------------------------------------
lambda_lasso = read.csv("results/lasso_lambda.csv") %>% .[1, 2]
fc_lasso = forecast_glmnet(y, x, idx = idx, lambda = lambda_lasso, alpha = 1)

# elastik net -------------------------------------------------------------
lambda_el = read.csv("results/el_lambda.csv") %>% .[1, 2]
fc_el = forecast_glmnet(y, x, idx = idx, lambda = lambda_el, alpha = 0.9)


# group lasso -------------------------------------------------------------
#NB: MEGET LANGSOM
lambda_grp = read.csv("results/grp_lambda.csv") %>% .[1, 2]
fc_grp = forecast_gglasso(y, x, grp, lambda_grp)


# adaptive lasso med OLS vægte --------------------------------------------


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

  
  