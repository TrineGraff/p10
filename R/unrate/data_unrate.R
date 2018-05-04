source("../data/setup_data.R")

drops = c("UNRATE")
x_f = scale(data[ , !(colnames(data) %in% drops)], center = TRUE) 
x = scale(data[ , !(colnames(data) %in% drops)], scale = TRUE, center = TRUE) 
y = scale(data[, "UNRATE"], scale = FALSE, center = TRUE) 

x_train = x[1:idx,]
y_train = y[1:idx]

x_test = x[-c(1:idx),]
y_test = y[-c(1:idx)]
