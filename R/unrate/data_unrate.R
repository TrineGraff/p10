source("../data/setup_data.R")

drops = c("UNRATE")
x = scale(data[ , !(colnames(data) %in% drops)]) 
y = scale(data[, "UNRATE"], scale = FALSE) 

x_train = x[1:idx,]
y_train = y[1:idx]

x_test = x[-c(1:idx),]
y_test = y[-c(1:idx)]
