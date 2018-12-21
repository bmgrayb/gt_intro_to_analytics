#i do this at the start of every file
rm(list=ls())
set.seed(10)
library(ggplot2)
library(forecast)

#read in data file
temp_data<-read.table("data/temps.txt",header = TRUE)
 
#convert to vector
temp_vec<-as.vector(unlist(temp_data[,2:21]))
plot(temp_vec)

#convert to timeseries
temp_ts<-ts(temp_vec,start = 1996,frequency = 123)
plot(temp_ts)

#plot the decomposition of our timeseries data
autoplot(decompose(temp_ts))

#holtwinters model
hw_model<-HoltWinters(temp_ts,alpha = NULL, beta = NULL, gamma = NULL, seasonal = "multiplicative")

#alpha, beta, and gamma values
hw_model$alpha
hw_model$beta
hw_model$gamma

#plot the model
plot(hw_model)

#plot the forecast of the model with the 80 and 95 confidence level
temp_forecast<-forecast(hw_model,h=123,level=c(80,95))
plot(temp_forecast, ylim = c(-40,140))

#use ARIMA model as well
arima_model <- auto.arima(temp_ts)
plot(forecast(arima_model, h=123))

#convert to matrix to calculate cusum
m<-matrix(hw_model$fitted[,4],ncol=123)

#cusum function written from previous homework
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


avg<-colMeans(m)
mu0<-mean(avg)
sigma0<-sd(vg)
yr<-cusum(avg,mu0,sigma0,sigma0)

plot(yr[[1]], ylim = c(0,.2))
abline(h=sigma0, col="blue")


