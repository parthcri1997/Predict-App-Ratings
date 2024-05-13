w1<-read.csv("C:\\Users\\elegant\\Downloads\\data_set\\org_data\\org_googleplaystore_user_reviews.csv")

library(tm)
library(devtools)
library(tm.plugin.sentiment)

corpus <- Corpus(VectorSource(w1$Translated_Review))
n1 <- TermDocumentMatrix(corpus)

sub <- subjectivity(n1)
pol <- polarity(n1)

w1$sub <- sub
w1$pol <- pol

write.csv(w1, file = "R_review.csv",row.names=FALSE)


