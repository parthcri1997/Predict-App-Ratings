rm(list=ls())
data<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with mean_rating\\final\\mean_Google_.csv")
#data_NaN<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\all category data with nan only\\Final all Nan values in rating\\Google_mean_only_nan_.csv")

#a1<-data_NaN[,c(2:9,12)]
#results1 <- fastDummies::dummy_cols(a1)
#x1<-results1[,c(2:5,7,9,10:49)]

a<-data[,c(2,4:8,10:11,15)]
results <- fastDummies::dummy_cols(a)
x<-results[,c(2:5,7,9:49)]

set.seed(222)
ind <- sample(2, nrow(x), replace = T, prob = c(0.8, 0.2))
train <- x[ind==1,]
test <- x[ind==2,]
#rf <- randomForest(Rating ~ Category.Id + Reviews + Size + Installs_1 + Android_Ver + No_of_days_till_last_update + Content_Rating_Id + Final_Price + T_Type.Id, data = train)
library(randomForest)
rf <- randomForest(Rating ~ ., data = train)
library(caret)
p1 <- predict(rf, test)
#x1$Rating <- p1
#a1$Rating <- p1
#data_NaN$Rating <- p1

#write.csv(data_NaN, file = "File_info_clean.csv",row.names=FALSE)

mean((test$Rating - p1)^2) 
postResample(pred = p1, obs = test$Rating)

plot(test$Rating,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))