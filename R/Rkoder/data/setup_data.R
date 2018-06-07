## DATA
library(tidyverse)

data_raw = read.csv("../data/transformed_data.csv") %>% as.data.frame()

idx =  floor(0.80 * nrow(data_raw)) 
train_dato = data_raw$dato[1:idx]

test_dato = data_raw$dato[-c(1:idx)]

data = data_raw[, -c(1, 2)]
