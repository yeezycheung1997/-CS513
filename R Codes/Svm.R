#  Course          : CS-513-A FINAL PROJECT
#  Algorithm       : svm
#  Finished by     : Qianyi Zhang
#  CWID            : 10455276
##################################################

Sys.setlocale("LC_ALL", "English")

# Remove all object
remove(list=ls())

library('e1071')
filename<-file.choose()
data <- read.csv(filename)

# Delete some fake prices in data
data <- data[-which(data$price==0),]

data$room_type <- as.factor(data$room_type)
data$neighbourhood <- as.factor(data$neighbourhood)
data$neighbourhood_group <- as.factor(data$neighbourhood_group)

price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)

data <- data.frame(data, price_level)
data$price_level <- as.factor(data$price_level)

# Delete the useless column below:
#  1: id          2: name                   3: host_id                               4: host_name
#  7: latitude    8: longitude             10: price                                11: minimum_nights
# 12: number_of_reviews                    13: last_review                          14: reviews_per_month
# 15: calculated_host_listings_count       16: availability_365
data <- data[,-c(1:4, 7:8, 10:16)]
summary(data)

idx <- sort(sample(nrow(data),as.integer(.3*nrow(data))))
training <- data[-idx,] # train 70% of the data
test <- data[idx,] # test 30% of the data

svm_model <- svm(price_level~.,data=training)
svm_model_pred <- predict(svm_model,training[,-4])
svm_table <- table(pred=svm_model_pred,true=training[,4])
svm_table
accuracy_1 <- sum(diag(svm_table))/sum(svm_table)
accuracy_1

svm_model_pred_2 <- predict(svm_model,test[,-4])
svm_table_2 <- table(pred=svm_model_pred_2,true=test[,4])
svm_table_2
accuracy_2 <- sum(diag(svm_table_2))/sum(svm_table_2)
accuracy_2

plot(svm_model,test)
