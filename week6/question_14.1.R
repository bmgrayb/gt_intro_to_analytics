rm(list=ls())
set.seed(1)
library(caret)
library(kernlab)

#read in data
cancer_data<-read.table("data/breast_cancer_data.txt", header = FALSE, na.strings = "?", sep = ",")

#column names
colnames(cancer_data) <- c("SampleNo", 
                    "Thickness", 
                    "UniformitySize", 
                    "UniformityShape", 
                    "Adhesion", 
                    "SingleEpithelialSize", 
                    "BareNuclei", 
                    "BlandChromatin", 
                    "NormalNucleoli", 
                    "Mitoses", 
                    "Class")

summary(cancer_data)

#it looks like there are 16 rows all in the BareNuclei column that are empty
#retrieve the rows with the missing data
missing<-which(is.na(cancer_data$BareNuclei),arr.ind = T)

cat("\nPercent missing: ", (length(missing)/nrow(cancer_data))*100, "%\n")


##################
# Mode imputation 
##################

#BareNuclei appears to be categorical since it can only be 1-10, so lets do Mode imputation
#using function to find mode from here: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#get the mode
mode_barenuclei<-as.numeric(mode(cancer_data[-missing,"BareNuclei"]))

#create new data set with imputed values
mode_cancer_data<-cancer_data
mode_cancer_data[missing,"BareNuclei"]<-mode_barenuclei


##############################
# Linear Regression Imputation
##############################

#use stepwise regression to get the most important predictors
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
lm_stepwise <- train(BareNuclei ~ ., data = cancer_data[-missing,2:ncol(cancer_data)], "lmStepAIC", scope = 
                      list(lower = BareNuclei~1, upper =BareNuclei ~ .), direction = "backward",trControl=ctrl)
summary(lm_stepwise)

#create model using predictors from stepwise regression
model_step<-lm(BareNuclei~NormalNucleoli + UniformitySize + UniformityShape + BlandChromatin + Adhesion + Class, 
               data = cancer_data[-missing,2:ncol(cancer_data)])

#get predictions using model
nuclei_preds<-predict(model_step,newdata = cancer_data[missing,2:ncol(cancer_data)])

#round the predictions so they are integers
nuclei_preds<-round(nuclei_preds)

#create new data set using predictions from model
lm_cancer_data<-cancer_data
lm_cancer_data[missing,"BareNuclei"]<-round(nuclei_preds)

################################
# regression with perturbation
################################

#get a new vector with random normally distributed values from our previous predictions
nuclie_perturbs<-rnorm(length(missing), mean(nuclei_preds), sd(nuclei_preds))

perturb_cancer_data<-cancer_data
perturb_cancer_data[missing,"BareNuclei"]<-round(nuclie_perturbs)

#create a function that takes in a train data set and a test data set and return the accuracy
svm_model<-function(train_data,test_data){
  
  model<-ksvm(Class~.,
              train_data,
              type="C-svc",
              kernal="rbfdot",
              C=100000,
              scaled=TRUE
  )
  
  pred<-predict(model,test_data[,1:10])
  acc<-sum(pred==test_data$Class)/nrow(test_data)
  
  return (acc)
} 


data_sets<-list(mode_cancer_data,lm_cancer_data,perturb_cancer_data,cancer_data[-missing,])
accs_vec<-rep(0,length(data_sets))

#loop over all the data sets, create the train and test sets, and then get the 
#accuracy of the model for each created data set
for(i in 1:length(data_sets)){
  
  train_ind<-sample(nrow(data_sets[[i]]), size = floor(nrow(data_sets[[i]]) * 0.7)) 
  #training data set
  train_data<-data_sets[[i]][train_ind,]
  #remaining data
  test_data<-data_sets[[i]][-train_ind,]
  
  accs_vec[i]<-svm_model(train_data, test_data)
  
}

cat("\nAccuracy in Mode imputation cancer data: ", accs_vec[1]*100, "%\n")
cat("Accuracy in Regression imputation cancer data: ", accs_vec[2]*100, "%\n")
cat("Accuracy in Regression with Perturbation imputation cancer data: ", accs_vec[3]*100, "%\n")
cat("Accuracy in cancer data set without missing data: ", accs_vec[4]*100, "%\n")
