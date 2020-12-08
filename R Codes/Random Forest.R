#  Course          : CS-513-A FINAL PROJECT
#  Algorithm       : Random Forest
#  Finished by     : Zichong Wong
#  CWID            : 10464881
##################################################
rm(list = ls())
data1=read.csv("/Users/zichongwang/Desktop/Fall2020 Semester/CS513/Final_Project/AB_NYC_2019.csv", na.strings = '?', colClasses=c(
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

library('randomForest')
set.seed(135)
index<-sort(sample(nrow(dataset),round(0.25*nrow(dataset))))
training<-dataset[-index,]
testing<-dataset[index,]
randomForest_class<-randomForest(price~ room_type+latitude+longitude+neighbourhood_group,data = training, importance=TRUE, ntree=1000)
summary(randomForest_class)
plot(randomForest_class)
randomForest_predict<-predict( randomForest_class ,testing , type="class" )

randomForest_predict
fit_importance <- importance(randomForest_class)
fit_importance

# Get top 15 variables from importance - these will be used in future algoritms
top_features <- sort(fit_importance[,1], decreasing=TRUE)[1:4]
top_features

# Check Results
varImpPlot(randomForest_class)
Prediction <- predict(randomForest_class, testing)

library(caret)


conf_matrix<-table(testing$price, Prediction)
confusionMatrix(conf_matrix)
str(Prediction)
str(testing$price)
