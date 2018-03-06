source("setup_data.R")

data = data_train[, -c(1,2)]
lapply(data, as.numeric)
corrplot(cor(data), order = "hclust", tl.col='black', tl.cex=.5) #kan intet se

nfact_carcass_fa = psych::nfactors(data, rotate = "none", fm = "mle")


plot(nscree_self <- nFactors::nScree(data))
nFactors::nScree(data)

Fa_regr <- factanal(data, factors = 3, rotation = "none", na.action = na.omit, scores = "regression", lower = 0.01)



FA_bart <- factanal(data, factors = 4, rotation = "none", start = NULL)
?factanal
