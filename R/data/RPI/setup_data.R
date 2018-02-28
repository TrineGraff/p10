## DATA
data = read.csv("transformed_data.csv") %>% as.data.frame()
data = slide(data, Var = 'RPI', NewVar = 'lag_RPI', slideBy = -1)  #lag værdi af responsen
data = na.omit(data)

idx =  floor(0.80 * nrow(data)) 
data_train = data[1:idx,]
data_test = data[-c(1:idx),]


y = scale(as.matrix(data[,"RPI"]))  %>% na.omit() 
y = y[-1] #fjerner en enkelt observation mere
drops <- c("RPI")
data = data[ , !(names(data) %in% drops)] #FJERNER RPI
X = scale(as.matrix(data[, -c(1,2)]))  %>% na.omit()


