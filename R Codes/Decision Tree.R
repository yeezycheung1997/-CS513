#  Course          : CS513 Knowledge Dis & Data Mining
#  Project         : final_project_Decision_Tree
#  First Name      : Ziming
#  Last Name       : Zhang
#  SWID            : 10455301

Sys.setlocale("LC_ALL", "English")

# remove all objects
rm(list=ls())

dev.off()
set.seed(721)

#install.packages("rpart")
#install.packages("rpart.plot")     
#install.packages("rattle")         
#install.packages("RColorBrewer") 

# Load Packages
library(rpart)
library(rpart.plot)		
library(rattle)											
library(RColorBrewer)

# import the dataset
filename<-file.choose()
data<-read.csv(filename)
View(data)

# Delete some fake prices in data
data <- data[-which(data$price==0),]

data$room_type <- as.factor(data$room_type)


price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

data <- data.frame(data, price_level)
data$price_level <- as.factor(data$price_level)

data <- data[,-c(1:6, 10:16)]
summary(data)
view(data)
#test data set and training data set
selected_index <- sort(sample(nrow(data),as.integer(.70*nrow(data))))
test_data <- data[selected_index, ]
training_data <- data[-selected_index,]
summary(test_data)
summary(training_data)

#Perform CART analysis
#	Generate prediction
Dtree<-rpart(price_level~.,data=training_data)
fancyRpartPlot(Dtree)

#	Test the	prediction
pred<-predict(Dtree	,test_data,	type="class")
table(Actual=test_data[,4],CART=pred)

# Compare the prediction to actual data and calculate the accuracy
right<-(test_data[,4]==pred)
accuracy<-sum(right)/length(right)
print(paste("Accuracy :" , accuracy))

