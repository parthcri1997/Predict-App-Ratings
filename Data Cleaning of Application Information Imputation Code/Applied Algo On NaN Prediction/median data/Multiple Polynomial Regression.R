rm(list=ls())
f<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with median_rating\\final\\Copy_final_median_.csv")
a<-f[,c(2:9,12)]   #a<-data[,c(2,4:8,10:11,15)]
str(f)
w <- a[,c(1:9)]
results <- fastDummies::dummy_cols(a)
w<-results[,c(2:5,7,9,10:49)]
set.seed(222)
ind <- sample(2, nrow(w), replace = T, prob = c(0.7, 0.3))
train <- w[ind==1,]
test <- w[ind==2,]

rf <- lm(train$Rating ~ poly(Reviews+Size+Installs_1+Final_Price+No_of_days_till_last_update+Category_FAMILY+Category_VIDEO_PLAYERS+Category_PHOTOGRAPHY+Category_NEWS_AND_MAGAZINES+Category_LIFESTYLE+Category_GAME+Category_FINANCE+Category_TOOLS+Category_FOOD_AND_DRINK+Category_PRODUCTIVITY+Category_LIBRARIES_AND_DEMO+Category_ENTERTAINMENT+Category_EVENTS+Category_MEDICAL+Category_PERSONALIZATION+Category_HOUSE_AND_HOME+Category_BEAUTY+Category_BOOKS_AND_REFERENCE+Category_COMMUNICATION+Category_HEALTH_AND_FITNESS+Category_SHOPPING+Category_DATING+Category_COMICS+Category_TRAVEL_AND_LOCAL+Category_WEATHER+Category_SPORTS+Category_PARENTING+Category_MAPS_AND_NAVIGATION+Category_BUSINESS+Category_SOCIAL+Category_EDUCATION+Category_ART_AND_DESIGN+Type_Free+Content_Rating_Everyone+Content_Rating_Teen+Content_Rating_Mature_17+Content_Rating_Everyone_10),3,raw = TRUE,data = train)
library(caret)
p1 <- predict(rf, test)
x1<- mean((test$Rating-p1)^2) 

plot(test$Rating,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

postResample(pred = p1, obs = test$Rating)