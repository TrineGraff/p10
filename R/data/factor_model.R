library(matrixcalc)
library(tidyverse)

df <- read.csv("/Users/LouiseNygaardChristensen/Desktop/P10/R/data/transformed_data.csv")
# 1 jan 1960 til 1 juli 2017 - svarende til 691 obs (dvs næsten 58 år)

# opdeler data i træningsmængde og testmængde
idx =  floor(0.80 * nrow(df)) 
data_train = df[1:idx, -c(1, 2)]
data_test = df[-c(1:idx), -c(1, 2)]

# forecaster arbejdsløsheden
y <- data_train$UNRATE
X <- data_train[ , !(names(data_train) %in% "UNRATE")]


data_train1 <- df[1:idx, -c(1)]
X.df <- data_train1[ , !(names(data_train) %in% "UNRATE")]

# Funktioner til at estimere faktorer i faktor-modellen -------------------

getFactors <- function(X, r) {
  n.var <- ncol(X)
  XTX <- crossprod(X)
  
  X.eig <- eigen(XTX, symmetric = TRUE)
  eig.vec <- X.eig$vectors
  
  loadings <- eig.vec[, 1:r] * sqrt(n.var)
  factors <- (X %*% loadings) / n.var
  
  list.out <- list(
    "factors" = factors,
    "loading" = loadings
  )
  return(list.out)
}

estFactors <- function(X, ic = 1, trace = FALSE) {
  X <- as.matrix(X.df[, -1])
  X <- scale(X)
  n.obs <- nrow(X)
  n.var <- ncol(X)
  
  r.max <- floor(10*log10(n.var))
  ics <- rep(NA, r.max)
  
  for (r in 1:r.max) {
    est.r <- getFactor(X, r)
    F.r <- est.r$factors
    L.r <- est.r$loading
    
    if(ic == 1) {
      penalty <- r * (n.var + n.obs) / (n.var * n.obs) * log((n.var * n.obs) / n.var + n.obs)
    } else if (ic == 2) {
      penalty <- r * (n.var + n.obs) / (n.var * n.obs) * log(min(n.var, n.obs))
    } else if (ic == 3) {
      penalty <- r * log(min(n.var, n.obs)) / min(n.var, n.obs)
    } else {
      stop("Invalid information criterion argument")
    }
    
    V.r <- matrix.trace(crossprod(X - tcrossprod(F.r, L.r))) / (n.obs * n.var)
    ics[r] <- log(V.r) + penalty
    if (trace) cat("r =", r, "\tV =", V.r, "\tIC =", ics[r], "\n")
  }
  
  r.opt <- which.min(ics)
  print(r.opt)
  est.opt <- getFactor(X, r.opt)
  factors.opt <- est.opt$factors
  colnames(factors.opt) <- paste("F", 1:r.opt, sep = "")
  
  df.out <- data.frame(X.df[, 1], factors.opt) %>% tbl_df
  return(df.out)
}

estFactors(X, ic = 1, trace = TRUE)

# Functioner til at forecaste med faktor-modellen -------------------------

factorForecast <- function(y, X.df, lags.df, h, ic = 1) {
  #n.obs <- X.df %>% dim %>% .[1]
  #p.max <- lags.df %>% dim %>% .[2] - 1
  n.obs <- nrow(X.df)
  p.max <- ncol(X.df) - 1
  F.df <- estFactors(X.df, ic)
  #r <- dim(F.df)[2] - 1
  r <- ncol(F.df) - 1
  
  ones.df <- data.frame(X.df$dato, 1) %>% tbl_df
  names(ones.df) <- c("dato", "skæring")
  
  fit.resp <- y[(1+h):n.obs]
  AICs <- rep(NA, (p.max + 1))
  
  for (p in 0:p.max) {
    lags.p <- lags.df[, 1:(1+p)]
    model.mat <- ones.df %>%
      left_join(F.df, by = "dato") %>%
      left_join(lags.p, by = "dato") %>%
      select(-dato) %>% as.matrix
    fit.mat <- model.mat[1:(n.obs-h), ]
    
    beta <- solve(crossprod(fit.mat), crossprod(fit.mat, fit.resp))
    resids <- fit.resp - fit.mat %>% beta
    sigma2 <- mean(resids^2)
    AICs[p+1] <- log(sigma2) + (n.obs + 2 * p) / n.obs
  }
  
  p.opt <- which.min(AICs) - 1
  model.mat <- ones.df %>%
    left_join(F.df, by = "dato") %>%
    left_join(lags.df[, 1:(1+p.opt)], by = "dato") %>%
    select(-dato) %>%
    as.matrix
  fit.mat <- model.mat[1:(n.obs-h), ]
  beta = solve(crossprod(fit.mat), crossprod(fit.mat, fit.resp))
  
  fc.vars <- drop(model.mat %>% tail(n = 1))
  y.fc <- drop(fc.vars %*% beta)
  
  list.out <- list(
    "forecast" = y.fc,
    "r" = r,
    "p" = p.opt
  )
  return(list.out)
}

factorForecast(y, X.df, lags.df, h = 1, ic = 1)