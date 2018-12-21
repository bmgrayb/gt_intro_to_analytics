#clear environment
rm(list=ls())

library(kknn)

cc_data<-read.table("data/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#inspect data
head(cc_data)

#****setting seed, don't need it for these models, but including it for good practice
set.seed(10)

#creating a function that allows you to pass in a value of K, and it returns the accuracy for the 
#model for that value
kknn_model<-function(K){
  preds<-rep(0,nrow(cc_data))
  
  # have to loop over every row in the data to get nearest neighbor
  for(i in 1:nrow(cc_data)){
    
    #train data all but current data point, test data is rest of the data points
    model<-kknn(V11~.,cc_data[-i,],
                cc_data[i,],
                k=K,
                scale=TRUE)
    preds[i]<-as.integer(fitted(model)+0.5) 
    
  }
  
  return (sum(preds == cc_data$V11) / nrow(cc_data))
}

#create a variable to keep track of the best K and accuracy
best_k<-0
best_acc<-0
k_max<-20
all_acc<-rep(0,k_max)

#loop over values of K to find the best accuracy
for(k in 1:k_max){
  all_acc[k]<-kknn_model(k)
  
  #store best accuracy and best K
  if(all_acc[k] > best_acc){
    best_acc = all_acc[k]
    best_k = k
  }
}

#output all accuracies, the best K and corresponding accuracy
all_acc
best_k
best_acc
