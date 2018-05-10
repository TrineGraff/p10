source("../data/setup_data.R")

drops = c("UNRATE")
xf = scale(data[ , !(colnames(data) %in% drops)], center = TRUE) 
xf_train = xf[1:idx,]
xf_test = xf[-c(1:idx),]

x = scale(data[ , !(colnames(data) %in% drops)], scale = TRUE, center = TRUE) 
y = scale(data[, "UNRATE"], scale = FALSE, center = TRUE) 

x_train = x[1:idx,]
y_train = y[1:idx]

x_test = x[-c(1:idx),]
y_test = y[-c(1:idx)]
