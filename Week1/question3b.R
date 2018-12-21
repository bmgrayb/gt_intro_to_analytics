#clear environment
rm(list=ls())

#library containing svm model 
library(kernlab)

# df containing credit card data without headers
cc_data <- read.table("data/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#inspect data
head(cc_data)

#****setting seed, don't need it for these models, but including it for good practice
set.seed(10)

#splitting data set into 70% train, 15% validation, and 15% test
train_ind<-sample(nrow(cc_data), size = floor(nrow(cc_data) * 0.7)) 
#training data set
train_data<-cc_data[train_ind,]
#remaining data
other<-cc_data[-train_ind,]

#take half of the remaining indices to split between validation and test
val_ind<-sample(nrow(other), size = floor(nrow(other) * 0.5)) 

#create validation and test sets.
validation_data<-other[val_ind,]
test_data<-other[-val_ind,]

#***we know from question 2 that rbfdot at 10000 points has the best accuracy for SVM
#but we should verify that using training, validation, and test sets


#copy model code from question 2, but also add a parameter to pass in the data set to use
cc_model<-function(c_value, kernal_type,train_set, val_set){
  
  model<-ksvm(V11~.,train_set,
              type="C-svc",
              kernal=kernal_type,
              C=c_value,
              scaled=TRUE
  )
  
  pred<-predict(model,val_set[,1:10])
  acc<-sum(pred==val_set$V11)/nrow(val_set)
  
  return (acc)
} 

# here we are creating a vector with various values of C to test our model on
c_vals<-c(0.001,0.1, 1, 100, 1000,10000)

# this is a vector containing the kernals we want to test our 
kernals<-c("vanilladot", "rbfdot")

# variables to keep track of the index of the best kernal and best C value
best_c<-0
best_kernal<-0
curr_best_acc<-0
all_acc<-rep(0,length(c_vals)*length(kernals))

for(i in 1:length(c_vals)){
  for(j in 1:length(kernals)){
    all_acc[i*j] = cc_model(c_vals[i],kernals[j], train_data, validation_data)
    
    if(all_acc[i*j]  > curr_best_acc){
      curr_best_acc<-all_acc[i*j] 
      best_c = i
      best_kernal = j
    }
    
  }
}

cat("Best accuracy: ", curr_best_acc,"\n")
cat("Best kernal: ", kernals[best_kernal], "\n")
cat("Best C value: ", c_vals[best_c], "\n")

#output all the accuracies to validate
all_acc

#create the final model
final_model<-ksvm(V11~.,train_data,
                  type="C-svc",
                  kernal=kernals[best_kernal],
                  C=c_vals[best_c],
                  scaled=TRUE
)
pred<-predict(final_model,test_data[,1:10])
acc<-sum(pred==test_data$V11)/nrow(test_data)


# find a1…am
a <- colSums(final_model@xmatrix[[1]] * final_model@coef[[1]])
a
# calculate a0
a0 <- final_model@b
a0
acc
