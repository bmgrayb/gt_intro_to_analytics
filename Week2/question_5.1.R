#i do this at the start of every file
rm(list=ls())
set.seed(10)

library(outliers)
library(ggplot2)

#read in data file
crime_data<-read.table("data/us_crime_data.txt", header = TRUE)

#initial inspection
head(crime_data)
summary(crime_data)

boxplot(x=crime_data$Crime)

#plot the data based on the State vs the amount of Crime
ggplot(data=crime_data,aes(x = as.numeric(row.names(crime_data)), y = Crime)) + 
  geom_point() + geom_text(aes(label=ifelse(Crime>1500|Crime<500,as.character(Crime),'')),hjust=0, vjust=0) + 
  labs(x="State_ID") 


#using grubbs.test type=10
#tests if data set has 1 outlier statistically different than the other values
o10<-grubbs.test(crime_data$Crime, type = 10)
o10

#using grubbs.test type=11
#check if lowest and highest value are two outliers on opposite tails of sample
o11<-grubbs.test(crime_data$Crime, type = 11)
o11


