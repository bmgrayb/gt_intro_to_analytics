#i do this at the start of every file
rm(list=ls())
set.seed(10)
library(ggplot2)

#read in data file
temp_data<-read.table("data/temps.txt",header = TRUE)

#initial inspection
head(temp_data)

#this function takes in a data set, a C value, and a T value
#and calculates the cusum. It returns the dataframe of running sum
#and the "unofficial end of summer"
cusum<-function(df, mu, sigma, t){
  
  S1<-0
  
  calc<-rep(0,length(df))
  all<-rep(0,length(df))
  
  day<-0
  
  for(i in 1:length(df)){
    x<-df[i]
    curr<-S1 + mu - x - sigma
    
    S1<-max(c(0,curr))
    
    calc[i]<-S1
    
    if(S1 > t ){
      day<-i
      break
    }
    
  }
  
  return(list(calc,day))
}

#vector to store the number day for the end of summer
#for each year
yearly_days<-rep(0,ncol(temp_data)-1)

for(i in 2:ncol(temp_data)){
  year_data<-temp_data[,i]
  mu0<-mean(year_data)
  sigma0<-sd(year_data)
  
  yr<-cusum(year_data,mu0,sigma0,sigma0)
  
  #storing actual day number
  yearly_days[i-1]<-yr[[2]]
}

#create a data frame containing the day number and actual day of the year for all observations
#for the unofficial end of summer
day_of_year<-data.frame(yearly_days,temp_data[yearly_days,1],substr(colnames(temp_data[2:21]),2,5))

day_of_year

#now calculate the cusum of the yearly day data to see if Atlanta has gotten hotter and when
total_days<-cusum(yearly_days, mean(yearly_days), sd(yearly_days),sd(yearly_days))

#create a dataframe of the response to be plotted
yr<-data.frame(substr(colnames(temp_data[2:21]),2,5),total_days[[1]])

#plot the data so we can see where it goes above the Threshold
ggplot(data=yr,aes(x = yr[[1]], y = yr[[2]])) + geom_point() + 
  geom_hline(aes(yintercept = sd(yearly_days)), color="blue",linetype="dashed") +
  geom_text(aes(0,sd(yearly_days),label = "Threshold", vjust = -1, hjust=-1),color="blue") +
  geom_text(aes(label=ifelse(yr[[2]]>sd(yearly_days),as.character(yr[[2]]),'')),hjust=1, vjust=1) + 
  labs(y="St",x="Year") 

#actual year where there has been change
day_of_year[total_days[[2]],3]
