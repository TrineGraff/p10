data_raw = read.csv("../data/transformed_data.csv") %>% as.data.frame()
data_raw$S.P.500
idx =  floor(0.80 * nrow(data_raw)) 
data = data_raw[, -c(1, 2)]
drops = c("S.P.500")

y = scale(data[, "S.P.500"], scale = FALSE, center = TRUE)

## Anvendes i AR
ya_train = y[1:idx]
ya_test = y[-c(1:idx)]

ya_dato_train = data_raw$dato[1:idx]
ya_dato_test = data_raw$dato[-c(1:idx)]

## Anvendes i faktor model
xf = scale(data[ , !(colnames(data) %in% drops)], center = TRUE) 
xf_train = xf[1:idx,]
xf_test = xf[-c(1:idx),]
yf_train = y[1:idx]
yf_test = y[-c(1:idx)]

#tilføjer 4 laggede værdier
df.y.lags = foreach(i = 1:4, .combine = cbind) %do%{
  lag(data[, "S.P.500"], i) 
}
colnames(df.y.lags) = c("lag1", "lag2", "lag3", "lag4")

x = data.frame(data[,!(colnames(data) %in% drops) ], df.y.lags) %>% .[-c(1:4),] 
x = scale(x)

x_train = x[1:(idx - 4),]
y_train = y[5:idx]

dato_train = data_raw$dato[5:idx]

x_test = x[-c(1:(idx - 4)),]
y_test = y[-c(1:(idx))]

dato_test = data_raw$dato[-(1:idx)]


