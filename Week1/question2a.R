#clear environment
rm(list=ls())

#library containing svm model 
library(kernlab)

# df containing credit card data without headers
cc_data<-read.table("data/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#inspect data
head(cc_data)

#****setting seed, don't need it for these models, but including it for good practice
set.seed(10)

#creating a function that allows you to pass in your C value and kernal type
#it returns the accuracy for the given C and kernal
cc_model<-function(c_value, kernal_type){
  
  model<-ksvm(V11~.,cc_data,
       type="C-svc",
       kernal=kernal_type,
       C=c_value,
       scaled=TRUE
  )
  
  pred<-predict(model,cc_data[,1:10])
  acc<-sum(pred==cc_data$V11)/nrow(cc_data)

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

for(i in 1:length(c_vals)){
  for(j in 1:length(kernals)){
    temp = cc_model(c_vals[i],kernals[j])
    
    if(temp > curr_best_acc){
      curr_best_acc<-temp
      best_c = i
      best_kernal = j
    }
      
  }
}

cat("Best accuracy: ", curr_best_acc,"\n")
cat("Best kernal: ", kernals[best_kernal], "\n")
cat("Best C value: ", c_vals[best_c], "\n")

#Lets check to make sure the best values of C and kernal don't result in all 1's or 0's
final_model<-ksvm(V11~.,cc_data,
                   type="C-svc",
                   kernal=kernals[best_kernal],
                   C=c_vals[best_c],
                   scaled=TRUE
                  )
pred<-predict(final_model,cc_data[,1:10])

#output predictions
pred

# find a1…am
a <- colSums(final_model@xmatrix[[1]] * final_model@coef[[1]])
a
# calculate a0
a0 <- final_model@b
a0




