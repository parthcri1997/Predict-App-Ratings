rm(list=ls())
data<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with mean_rating\\final\\mean_Google_.csv")

a<-data[,c(2,4:8,10:11,15)]
results <- fastDummies::dummy_cols(a)
x<-results[,c(2:5,7,9:41,43,45:48)]

set.seed(222)
ind <- sample(2, nrow(x), replace = T, prob = c(0.8, 0.2))
train <- x[ind==1,]
test <- x[ind==2,]

rf <- lm(Rating ~ ., data = train)
library(caret)
p1 <- predict(rf, test)
mean((test$Rating - p1)^2) 
library(Metrics)
mse(test$Rating, p1)

plot(test$Rating,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

postResample(pred = p1, obs = test$Rating)