#clear environment
rm(list=ls())

library(kknn)

cc_data<-read.table("data/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#inspect data
head(cc_data)

#****setting seed, don't need it for these models, but including it for good practice
set.seed(10)

#create a variable to keep track of the best K and accuracy
best_k<-0
best_acc<-0
k_max<-20
all_acc<-rep(0,k_max)

#first method
#use train.knn for leave one out cv
model<-train.kknn(V11~.,cc_data,kmax=k_max,scale=TRUE)

for(i in 1:k_max){
  pred <- as.integer(fitted(model)[[i]][1:nrow(cc_data)] + 0.5) # round off to 0 or 1
  all_acc[i] <- sum(pred == cc_data$V11)/nrow(cc_data)
  
  #store best accuracy and best K
  if(all_acc[i] > best_acc){
    best_acc = all_acc[i]
    best_k = i
  }
}

all_acc
best_k
best_acc


#second method
#using cv.kknn for K fold cross validation

#create a variable to keep track of the best K and accuracy
best_k_kcv<-0
best_acc_kcv<-0
all_acc_kcv<-rep(0,k_max)

for (j in 1:k_max) {
  
  #k cross fold validation with 10 folds
  model_kcv <- cv.kknn(V11~.,cc_data,
                   kcv=10,
                   k=j, 
                   scale=TRUE)
  
  pred_kcv <- as.integer(model_kcv[[1]][,2] + 0.5) 
  all_acc_kcv[j] <- sum(pred_kcv == cc_data$V11)/nrow(cc_data)
  
  #store best accuracy and best K
  if(all_acc_kcv[j] > best_acc_kcv){
    best_acc_kcv = all_acc_kcv[j]
    best_k_kcv = j
  }
  
}

all_acc_kcv
best_k_kcv
best_acc_kcv

