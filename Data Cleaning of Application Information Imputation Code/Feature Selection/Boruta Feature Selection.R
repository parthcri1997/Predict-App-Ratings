rm(list=ls())
f<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with mean_rating\\final\\mean_Google_.csv")

a<-f[,c(1,2,4:8,10:11,13,14,15,16,17)]   #a<-data[,c(2,4:8,10:11,15)]
str(f)


library(Boruta)
boruta_output <- Boruta(a$Rating ~ ., data=a) 
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

