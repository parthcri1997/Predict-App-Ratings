rm(list=ls())
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
f<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with median_rating\\final\\Copy_final_median_.csv")
str(f)
a<-f[,c(2:9,12)]   #a<-data[,c(2,4:8,10:11,15)]
str(a)
results <- fastDummies::dummy_cols(a)
colnames(results)
w <- results[,c(2:5,7,9,10:49)]
w[is.na(w)]<-0
n <- neuralnet(Rating ~ Reviews+Size+Installs_1+Final_Price+No_of_days_till_last_update+Category_FAMILY+Category_VIDEO_PLAYERS+Category_PHOTOGRAPHY+Category_NEWS_AND_MAGAZINES+Category_LIFESTYLE+Category_GAME+Category_FINANCE+Category_TOOLS+Category_FOOD_AND_DRINK+Category_PRODUCTIVITY+Category_LIBRARIES_AND_DEMO+Category_ENTERTAINMENT+Category_EVENTS+Category_MEDICAL+Category_PERSONALIZATION+Category_HOUSE_AND_HOME+Category_BEAUTY+Category_BOOKS_AND_REFERENCE+Category_COMMUNICATION+Category_HEALTH_AND_FITNESS+Category_SHOPPING+Category_DATING+Category_COMICS+Category_TRAVEL_AND_LOCAL+Category_WEATHER+Category_SPORTS+Category_PARENTING+Category_MAPS_AND_NAVIGATION+Category_BUSINESS+Category_SOCIAL+Category_EDUCATION+Category_ART_AND_DESIGN+Category_AUTO_AND_VEHICLES+Type_Free+Type_Paid+Content_Rating_Everyone+Content_Rating_Teen+Content_Rating_Mature_17+Content_Rating_Everyone_10+Content_Rating_Adults_only_18
               , data = w, hidden = c(10,5), linear.output = FALSE, lifesign = 'full',rep = 1)


plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

w <- as.matrix(w)
dimnames(w) <- NULL
set.seed(222)
ind <- sample(2, nrow(w), replace = T, prob = c(0.7, 0.3))
training <- w[ind==1,2:46]
test <- w[ind==2,2:46]
trainingtarget <- w[ind==1,1]
testingtarget <- w[ind==2,1]

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)
------------------------------------------------------------------------------
  library(keras)
library(tensorflow)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape =c(45)) %>%
  layer_dense(units = 1)

model %>% compile(loss= 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(test, testingtarget)
pred <- model %>% predict(test)
mean((testingtarget-pred)^2)
plot(testingtarget,pred)
postResample(pred = pred, obs = testingtarget)

plot(testingtarget,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(pred,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

-----------------------------------------------------------------------------
  model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = 'relu', input_shape =c(45)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)

model %>% compile(loss= 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(test, testingtarget)
pred <- model %>% predict(test)
mean((testingtarget-pred)^2) 
plot(testingtarget,pred)
postResample(pred = pred, obs = testingtarget)


----------------------------------------------------------------------------  
  model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape =c(45)) %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate=0.3)%>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate=0.2)%>%
  layer_dense(units = 1)

model %>% compile(loss= 'mse',
                  #optimizer = 'rmsprop',
                  optimizer = optimizer_rmsprop(lr = 0.001),
                  metrics = 'mae')

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(test, testingtarget)
pred <- model %>% predict(test)
m1 <- mean((testingtarget-pred)^2)   
sqrt(m1)	
plot(testingtarget, pred)  

------------------------------------------------------------------------------
  #R^2 value
  rss <- sum((pred - testingtarget) ^ 2)  ## residual sum of squares
tss <- sum((testingtarget - mean(pred)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

------------------------------------------------------------------------------  
f$Rating <- (f$Rating - min(f$Rating))/(max(f$Rating)- min(f$Rating))
f$Reviews <- (f$Reviews - min(f$Reviews))/(max(f$Reviews)- min(f$Reviews))
f$Size <- (f$Size - min(f$Size))/(max(f$Size)- min(f$Size))
f$Installs_1 <- (f$Installs_1 - min(f$Installs_1))/(max(f$Installs_1)- min(f$Installs_1))
f$Final_Price <- (f$Final_Price - min(f$Final_Price))/(max(f$Final_Price)- min(f$Final_Price))
f$No_of_days_till_last_update <- (f$No_of_days_till_last_update - min(f$No_of_days_till_last_update))/(max(f$No_of_days_till_last_update)- min(f$No_of_days_till_last_update))
library(neuralnet)



