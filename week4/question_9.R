#i do this at the start of every file
rm(list=ls())
set.seed(10)

#read in data file
crime_data<-read.table("data/us_crime_data.txt",header = TRUE)

#recreate our model from the last homework
crime_model0.1<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data = crime_data)
summary(crime_model0.1)

#conduct principal component analysis
pca<-prcomp(crime_data[,1:15], scale.=TRUE)
summary(pca)

#combine pca with data
pca_data<-as.data.frame(cbind(pca$x[,1:10],crime_data$Crime))

#create a model using the first 10 principal components
crime_model_pca<-lm(V11~.,data=pca_data)
summary(crime_model_pca)

#translate coefficients into original terms
coef<-crime_model_pca$coefficients[2:length(crime_model_pca$coefficients)]%*%t(pca$rotation[,1:(length(crime_model_pca$coefficients)-1)])
intercept<-crime_model_pca$coefficients[1] - sum(coef*sapply(crime_data[,1:15],mean)/sapply(crime_data[,1:15],sd))
coef<-coef/sapply(crime_data[,1:15],sd)

#get predictions
preds<-as.matrix(crime_data[,1:15]) %*% t(coef) + intercept

#calculate r2
sse<-sum((preds-crime_data$Crime)^2)
sst<-sum((crime_data$Crime - mean(crime_data$Crime))^2)
r2<-1-sse/sst
r2adj<-1-(1-r2)*(nrow(crime_data)-1)/(nrow(crime_data)-1-10)

cat("R2: ", r2,"\n")
cat("R2 Adjusted: ", r2adj,"\n")
