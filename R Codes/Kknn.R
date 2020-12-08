#  Course          : CS-513-A FINAL PROJECT
#  Algorithm       : kknn
#  Finished by     : Qianyi Zhang
#  CWID            : 10455276
##################################################

Sys.setlocale("LC_ALL", "English")

# Remove all object
remove(list=ls())

library(kknn)
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

# k=3
predict_k3 <- kknn(formula=price_level~., training, test, k=3,kernel ="rectangular")
fit <- fitted(predict_k3)
table(test$price_level,fit)

# Measure the performance of knn
k3_wrong<- (test$price_level !=fit)
k3_wrong_rate<-sum(k3_wrong)/length(k3_wrong)
k3_wrong_rate

# Accuracy for test
k3_accuracy_rate <- 1-k3_wrong_rate
k3_accuracy_rate

# k=10
predict_k10 <- kknn(formula=price_level~., training, test, k=10,kernel ="rectangular")
fit <- fitted(predict_k10)
table(test$price_level,fit)

# Measure the performance of knn
k10_wrong<- (test$price_level !=fit)
k10_wrong_rate<-sum(k10_wrong)/length(k10_wrong)
k10_wrong_rate

# Accuracy for test
k10_accuracy_rate <- 1-k10_wrong_rate
k10_accuracy_rate
