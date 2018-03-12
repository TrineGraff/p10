## DATA
library(tidyverse)
library(DataCombine)

data = read.csv("transformed_data.csv") %>% as.data.frame()
data = slide(data, Var = 'RPI', NewVar = 'lag_RPI', slideBy = -1)  #lag værdi af responsen
data = na.omit(data)

idx =  floor(0.80 * nrow(data)) 



data_train = data[1:idx,]
data_test = data[-c(1:idx),]

y = scale(as.matrix(data_train[,"RPI"]))  %>% na.omit() 
#y = y[-1] #fjerner en enkelt observation mere
drops <- c("RPI")
data_train = data_train[ , !(names(data_train) %in% drops)] #FJERNER RPI
X = scale(as.matrix(data_train[, -c(1,2)]))  %>% na.omit()

#fjerner dato og X og scallerer
data_train = data_train[, -c(1,2)]
data_train = scale(as.matrix(data_train))

                     