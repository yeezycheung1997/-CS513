

rm(list=ls())

set.seed(123)
# Read data
filename<-file.choose()
data<-  read.csv(filename,colClasses = c("neighbourhood"="factor","neighbourhood_group"="factor"))
summary(data)

# Delete unrelated data
data <- data[-which(data$price == 0),c(7,8,9,10,11,12,14,15,16)]


# Label some attributes.
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)
for(i in 1 : length(data[,3])){
  if(data$room_type[i] =="Private room"){
    data$room_type[i] = 1;
  }else if(data$room_type[i] =="Entire home/apt"){
    data$room_type[i] = 2;
  }else if(data$room_type[i]=="Shared room"){
    data$room_type[i] = 3; 
  }else{
    data$room_type[i] = NA;
  }
}
for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}
data <- data.frame(data[,-4],price_level)
data$price_level <- as.factor(data$price_level)
data <- na.omit(data)

# Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
price_level <- data[,9]
data <- data.frame(lapply(data[,-9],normalize),price_level)

# Get training data and test data
index<-sort(sample(nrow(data),round(.3*nrow(data))))
training<- data[-index,]
test<- data[index,]


library("neuralnet")
?neuralnet

# Train neural network
nn<- neuralnet(price_level~.,training, hidden=15, threshold=0.5,stepmax=1e8,lifesign = "full")
plot(nn)

# Predict test data.
ann <- compute(nn,test[,-9])
ann$net.result 

# Label prediction by probability.
ann_cat<-ifelse(ann$net.result[,1] > 1/3,"[0, 100)",
                ifelse(ann$net.result[,2] > 1/3,"[100, 200)","[200, +)"))


# Get accuracy
table(Actual=test$price_level,predition=ann_cat)
wrong<- (test$price_level!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
accuracy <- 1 - error_rate
accuracy
