#i do this at the start of every file
rm(list=ls())
set.seed(10)
library(tree)
library(randomForest)
library(ggplot2)

#read in data file
crime_data<-read.table("data/us_crime_data.txt",header = TRUE)

#create tree model
crime_tree<-tree(Crime~.,data=crime_data)
summary(crime_tree)

#plot out the tree
plot(crime_tree)
text(crime_tree)

#calculate r2
y_hat<-predict(crime_tree)
ssr<-sum((y_hat-crime_data$Crime)^2)
sst<-sum((crime_data$Crime - mean(crime_data$Crime))^2)
r2<-1-ssr/sst

#create random forest
crime_rf<-randomForest(Crime~.,data=crime_data,mtry=4,importance=TRUE)
crime_rf

#get info about random forest
importance(crime_rf)
varImpPlot(crime_rf)
