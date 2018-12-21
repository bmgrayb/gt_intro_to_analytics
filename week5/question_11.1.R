rm(list=ls())
set.seed(1)
library(MASS)
library(glmnet)

#read in data, and create a second data set that is scla
crime_data<-read.table("data/us_crime_data.txt", header = TRUE)
crime_data_scaled<-crime_data

#check which columns are binary 
binary<-apply(crime_data_scaled,2,function(x){all(x %in% 0:1)})
binary

#scale all columns except for binary
crime_data_scaled[!binary] = scale(crime_data_scaled[!binary])

#create initial model with all unscaled data
model<-lm(Crime~., data=crime_data_scaled)

#create model using stepwise regression
model_stepwise<-stepAIC(model, direction = "both", trace = F)
model_stepwise$anova
summary(model_stepwise)

#get MSE for stepwise regression
mse_sw<-mean(model_stepwise$residuals^2)

#get x and y values for glmnet
xvals<-as.matrix(crime_data_scaled[,-16])
yvals<-as.matrix(crime_data_scaled[,16])


#get training and test set for cross validation
train_ind<-sample(nrow(crime_data_scaled), size=floor(nrow(crime_data_scaled) * .7))
xtrain<-crime_data_scaled[train_ind,1:15]
ytrain<-crime_data_scaled[train_ind,16]
xtest<-crime_data_scaled[-train_ind,1:15]
ytest<-crime_data_scaled[-train_ind,16]

#create lasso model alpha=1
model_lasso<-glmnet(x=xvals,y=yvals,alpha = 1)
par(mfrow=c(1,2))
plot(model_lasso,xvar = "lambda")
plot(model_lasso, xvar = "norm")

#cross validation of lasso
lasso_cv<-cv.glmnet(as.matrix(xtrain), ytrain,alpha=1, nfolds = 5,
                   type.measure = "mse",family = "gaussian", standardize = FALSE )

#plot our MSE vs lambda
plot(lasso_cv)

best_lambda <- lasso_cv$lambda.min
coef(lasso_cv, s = "lambda.min")

# Check MSE
yhat <- predict(lasso_cv, s=lasso_cv$lambda.min, newx=as.matrix(xtest))
mse_lasso <- mean((ytest - yhat)^2)


#same process using Elastic Net a=0 is ridge regression. 
model_en<-glmnet(x=xvals,y=yvals,alpha = 0)
par(mfrow=c(1,2))
plot(model_en,xvar = "lambda")
plot(model_en, xvar = "norm")

#cross validation of elastic net
en_cv<-cv.glmnet(as.matrix(xtrain), ytrain,alpha=0, nfolds = 5,
                    type.measure = "mse",family = "gaussian", standardize = FALSE )

#plot our MSE vs lambda
plot(en_cv)

best_lambda_en <- en_cv$lambda.min
coef(en_cv, s = "lambda.min")

# Check MSE
yhat_en <- predict(en_cv, s=en_cv$lambda.min, newx=as.matrix(xtest))
mse_en <- mean((ytest - yhat_en)^2)

