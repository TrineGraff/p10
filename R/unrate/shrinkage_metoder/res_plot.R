qqnorm.plot = function(y){
  q.sample_l = quantile(y)[["25%"]]
  q.sample_u = quantile(y)[["75%"]]
  q.theory_l = qnorm(0.25)
  q.theory_u = qnorm(0.75)
  slope = (q.sample_l - q.sample_u)/(q.theory_l-q.theory_u)
  int = q.sample_l - slope*q.theory_l
  ggplot() +
    stat_qq(aes(sample = y)) +
    geom_abline(intercept = int, slope = slope) +
    xlab("Teoretisk kvantil") +
    ylab("Sample kvantil") +
    ggtitle("Normal Q-Q plot") 
}

histogdens.plot = function(y){
  ggplot(mapping = aes(x = y)) +
    geom_histogram(aes(y = ..density..), binwidth = 1) +
    geom_density() +
    stat_function(fun = dnorm, color="red", args=list(mean=mean(y), sd=sd(y))) +
    xlab("Standard afvigelse") +
    ylab("") +
    ggtitle("Fordelingen af residualerne")
}

residuals.plot = function(y) {
  ggplot(tmp, aes(Date, y)) + 
    geom_line() +
    xlab("Dato") +
    ylab("Residualer") +
    ggtitle("Residualerne")
}

residuals.acf.plot = function(y){
  acf <- acf(y, plot = FALSE)
  df <- with(acf, data.frame(lag, acf))
  acf.confint <- qnorm(0.975) / sqrt(length(y))
  acf.plot <- df %>%
    ggplot(aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = c(-acf.confint, acf.confint), linetype = "dashed", color = "blue") +
    xlab("Lags") +
    ylab("Autokorrelation") +
    ggtitle("Residual korrelogram")
}
