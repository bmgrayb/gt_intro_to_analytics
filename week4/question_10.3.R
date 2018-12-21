#i do this at the start of every file
rm(list=ls())
set.seed(10)
library(pscl)
library(ggplot2)
library(reshape2)

#read in data
credit_data<-read.table("data/german_credit.txt")

#1's and 2's responses to 0's and 1's
credit_data$V21[credit_data$V21==1]<-0
credit_data$V21[credit_data$V21==2]<-1

#create logistic regression model
credit_model<-glm(V21~.,famil=binomial(link="logit"), data=credit_data)
summary(credit_model)
credit_model$coefficients

#psuedo-r2 value using the McFadden column
pR2(credit_model)

#plot model
layout(matrix(c(1,2,3,4),2,2))
plot(credit_model)

#split data into training and testing set
train_ind<-sample(nrow(credit_data), size = floor(nrow(credit_data) * 0.7)) 
train_data<-credit_data[train_ind,]
test_data<-credit_data[-train_ind,]

#create another logistic regression model using our training data
#we will also only use predictors with a pvalue < 0.1 from part 1
credit_model_2<- glm(V21~V1+V2+V3+V4+V5+V6+V8+V9+V10+V14+V20, family=binomial(link="logit"), train_data)
summary(credit_model_2)
pR2(credit_model_2)

#get predictions
preds<-predict(credit_model_2,test_data[,1:20])

#writing a function that returns a list of correct, false positives, and false negatives
calc_correct<-function(data_set,predicted,threshold){
  
  fp<-0
  fn<-0
  correct<-0
  
  for(i in 1:nrow(data_set)){
    curr<- -1
    if(predicted[i] >= threshold){
      curr<-1
    }
    else{
      curr<-0
    }
    
    if(data_set[i,21] == curr){
      correct<-correct+1
    }
      
    if(data_set[i,21] == 0 & data_set[i,21] != curr){
      fp<-fp+1
    }
    if(data_set[i,21] == 1 & data_set[i,21] != curr){
      fn<-fn+1
    }
    
  }
  
  return (list(correct,fp,fn))
}

#variables for threshold and the response
t<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5)
Cost<-rep(0,length(t))
Correct<-rep(0,length(t))
FalsePositives<-rep(0,length(t))
FalseNegatives<-rep(0,length(t))

#test for all values of threshold
for(i in 1:length(t)){
  results<-calc_correct(test_data,preds,t[i])
  Cost[i]<-5*results[[2]] + results[[3]]
  Correct[i]<-results[[1]]
  FalsePositives[i]<-results[[2]]
  FalseNegatives[i]<-results[[3]]
}

#create a dataframe and manipulate it to be plotted
df<-data.frame(t,Cost,Correct,FalsePositives,FalseNegatives)
df_long<-melt(df,id="t")

#plot
ggplot(data=df_long,aes(x=t,y=value,color=variable)) +
  geom_point()+
  geom_text(aes(label=value),hjust=-.5, vjust=1)+
  labs(x="Threshold")+
  stat_smooth(method="lm", se=FALSE)+
  xlim(.05,1.55)


