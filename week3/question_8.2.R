#i do this at the start of every file
rm(list=ls())
set.seed(10)
library(ggplot2)
library(smooth)
library(DAAG)

#read in data file
crime_data<-read.table("data/us_crime_data.txt",header = TRUE)

#create linear regression model
crime_model<-lm(Crime~.,data = crime_data)

#plot all outputs from the linear model
layout(matrix(c(1,2,3,4),2,2))
plot(crime_model)
summary(crime_model)

#create a new model with only predictors with p value < 0.1
#this is to counteract overfitting
crime_model0.1<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data = crime_data)
plot(crime_model0.1)
summary(crime_model0.1)

#test point
test_pt <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, 
                  M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, 
                  Ineq = 20.1, Prob = 0.040,Time = 39.0)

#create a prediction for the point using both models
prediction_1<-predict(crime_model,test_pt)
prediction_2<-predict(crime_model0.1,test_pt)

#output our predictions
prediction_1
prediction_2

#create new observations with our predicted amount of Crime
pred_pt1<-test_pt
pred_pt1$Crime<-prediction_1
pred_pt2<-test_pt
pred_pt2$Crime<-prediction_2

#append the new rows to our dataframe
new_crime_data<-rbind(crime_data,pred_pt1)
new_crime_data<-rbind(new_crime_data,pred_pt2)

#plot our crime data including the new predictions
ggplot(data=new_crime_data,aes(x = as.numeric(row.names(new_crime_data)), y = Crime)) + 
  geom_point(color=ifelse(as.numeric(row.names(new_crime_data))>47,"red","black")) + 
  geom_text(aes(label=ifelse(Crime>1500|Crime<500,as.character(Crime),'')),hjust=0, vjust=0) + 
  labs(x="State_ID")

#cross validate the second model
crime_model0.1_cv<-cv.lm(crime_data,crime_model0.1,m=5,seed=10)

#calculate R-squared
ssr<-attr(crime_model0.1_cv,"ms")*nrow(crime_data)
sst<-sum((crime_data$Crime-mean(crime_data$Crime))^2)

r2<-1-ssr/sst

