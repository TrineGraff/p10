## DATA
library(tidyverse)

data_raw = read.csv("../data/transformed_data.csv") %>% as.data.frame()

idx =  floor(0.80 * nrow(data_raw)) 
data_train = data_raw[1:idx,]
train_dato = data_train$dato
data_train = data_train[, -c(1, 2)]

data_test = data_raw[-c(1:idx),]
test_dato = data_test$dato
data_test = data_test[, -c(1, 2)]

data = data_raw[, -c(1, 2)]
