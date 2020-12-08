

rm(list=ls())


filename<-file.choose()
data<-  read.csv(filename,colClasses = c("neighbourhood"="factor","neighbourhood_group"="factor","room_type"="factor"))
summary(data)

data <- data[-which(data$price == 0),c(5,6,9,10,11,12,14,15,16)]
price_level <- ifelse(
  data$price<100, "[0, 100)",
  ifelse(
    data$price<200, "[100, 200)","[200, +)"
  )
)
for(i in 1 : length(data)){
  data[,i]<- as.numeric(data[,i])
}
table(data$price_level)
data <- data.frame(data[,-4],price_level)
data$price_level <- as.factor(data$price_level)
data <-data.frame(lapply(na.omit(data),as.numeric))
data <- na.omit(data)
data <- data[,c(1,2,3,9)]
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
price_level <- data[,4]
data <- data.frame(lapply(data[,-4],normalize),price_level)

index<-sort(sample(nrow(data),round(.3*nrow(data))))
training<- data[-index,]
test<- data[index,]

for(i in 1 : length(training[,4])){
  training[i,4] <- training[i,4]-1
}
for(i in 1 : length(test[,4])){
  test[i,4] <- test[i,4]-1
}

test<- data[index,]


library("neuralnet")
?neuralnet
nn<- neuralnet(price_level~.,training, hidden=15, threshold=0.1,stepmax=1e8,lifesign = "full")
plot(nn)


ann <- compute(nn,test[,-4])
ann$net.result 

ann_cat<-ifelse(ann$net.result <0.5,0,
                ifelse(ann$net.result<1.5,1,2))
length(ann_cat)

table(Actual=test$price_level,predition=ann_cat)

wrong<- (test$price_level!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
accuracy <- 1 - error_rate
accuracy
