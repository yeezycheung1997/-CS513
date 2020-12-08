
rm(list=ls())

library(class)

file <- file.choose()
data <- read.csv(file)

# Delete useless data for KNN we select latitude/longitude(location)/room type/minimum_nights/numbers of reviews/reviews per month
# /calculated host listings count/availability 365 as valid data to predict the price.
data <- data[-which(data$price == 0),c(7,8,9,10,11,12,14,15,16)]

# label room types as numeric
for(i in 1 : length(data[,3])){
  if(data$room_type[i] =="Private room"){
    data$room_type[i] = 1;
  }else if(data$room_type[i] =="Entire home/apt"){
    data$room_type[i] = 2;
  }else if(data$room_type[i]=="Shared room"){
    data$room_type[i] = 3; 
  }
}
for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}

# Delete error data rows
data <- na.omit(data)

# label price range
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)
data <- data.frame(data[,-4])
summary(data)

# Normalize data based on Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
data <- data.frame(lapply(data,normalize),price_level)
data$price_level <- as.factor(data$price_level)

# Get 70% data as training set, 30% data as testing set.
idx <- sort(sample(nrow(data),as.integer(.3*nrow(data))))
training <- data[-idx,] # train 70% of the data
test <- data[idx,] # test 30% of the data

# knn Prediction
predict <- knn(training[,-9], test[,-9], training$price_level,k=50)

# Get accuracy.
table(Actual=test[,9],KNN=predict)
accuracy <-sum(test[,9] == predict) / length(test[,1])
accuracy



#k    Accuracy
#5    0.677
#10   0.686
#30   0.693
#50   0.694   ¡Ì
#80   0.692
#100  0.690      
#130  0.689
#150  0.687
#200  0.685