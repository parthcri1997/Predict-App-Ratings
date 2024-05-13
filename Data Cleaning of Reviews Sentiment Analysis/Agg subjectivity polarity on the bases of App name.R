w1<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\Data Cleaning of Reviews\\R Prediction\\Agg_Review.csv")
length(unique(w1$App))
w1<-aggregate(w1,by = list(w1$App),FUN = mean)
w1 <- w1[,c(1,3,4)]
colnames(w1) <- c("App", "Sub","Pol")

w1$Sentiment<-ifelse(w1$Pol > 0,"Positive",
                     ifelse(w1$Pol == 0,"Neutral",
                            ifelse(w1$Pol < 0,"Negative","Null")))

write.csv(w1, file = "Agg_R_review.csv",row.names=FALSE)

