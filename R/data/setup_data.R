## DATA
library(tidyverse)
library(DataCombine)

data = read.csv("transformed_data.csv") %>% as.data.frame()
data = na.omit(data)

idx =  floor(0.80 * nrow(data)) 
data_train = data[1:idx,]
train_dato = data_train$dato
data_train = scale(as.matrix(data_train[, -c(1, 2)]))

data_test = data[-c(1:idx),]
test_dato = data_test$dato
data_test = scale(as.matrix(data_test[, -c(1, 2)]))

