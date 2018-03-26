## DATA
library(tidyverse)
library(DataCombine)

data = read.csv("/Users/trinegraff/Desktop/Projekt/R/data/transformed_data.csv") %>% as.data.frame()

idx =  floor(0.80 * nrow(data)) 
data_train = data[1:idx,]
train_dato = data_train$dato
data_train = scale(data_train[, -c(1, 2)])

data_test = data[-c(1:idx),]
test_dato = data_test$dato
data_test = scale(data_test[, -c(1, 2)])

