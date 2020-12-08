rm(list = ls())
filename<-file.choose()
data1=read.csv(filename, na.strings = '?', colClasses=c(
  "neighbourhood_group"="factor","neighbourhood"="factor","room_type"="factor"))
View(data1)

data1 <- data1[-which(data1$price==0),]
data1$price <- ifelse(
  data1$price<100, "[0, 100)",
  ifelse(
    data1$price<200, "[100, 200)","[200, +)"
  )
)


data1$price <- as.factor(data1$price)
dataset = data1[,-c(1:4,11:16)]
summary(dataset)
library(randomForest)
library(dbplyr)
library(C50)
idx<-sort(sample(nrow(dataset),as.integer(.75*nrow(dataset))))
training<-dataset[idx,]
test<-dataset[-idx,]
features<-c('room_type','latitude','longitude','neighbourhood_group','price')
model<-C5.0(price~. , training[,features])
summary(model)
plot(model)
#Prediction using test 
prediction<-predict(model,test,type="class") 
#Forming the confusin matrix
conf_matrix<-table(test[,],prediction)
conf_matrix
str(prediction)


Prediction <- predict(model, test)
library(caret)
confusionMatrix(test$price, Prediction)
