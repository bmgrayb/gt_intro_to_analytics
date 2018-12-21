#i do this at the start of every file
rm(list=ls())
set.seed(10)

library(ggplot2)
library(gridExtra)

# read in data
iris_data<-read.table("data/iris_data.txt", header = TRUE)

#initial ispection of data
head(iris_data)
summary(iris_data)

# plot the data for different attributes
p1<-ggplot(data=iris_data, aes(Petal.Length, Petal.Width, color= Species)) + geom_point()
p2<-ggplot(data=iris_data, aes(Sepal.Length, Sepal.Width, color= Species)) + geom_point()
p3<-ggplot(data=iris_data, aes(Petal.Length, Sepal.Length, color= Species)) + geom_point()
p4<-ggplot(data=iris_data, aes(Petal.Width, Sepal.Width, color= Species)) + geom_point()
p5<-ggplot(data=iris_data, aes(Petal.Width, Sepal.Length, color= Species)) + geom_point()
p6<-ggplot(data=iris_data, aes(Petal.Length, Sepal.Width, color= Species)) + geom_point()

grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)

k_max<-20
d<-rep(0,k_max)

#after viewing plots, it looks like the most distinct separation is between
#petal length vs petal width. So we can scale the data and only select those
#columns to be used for clustering
scaled<-scale(iris_data[,3:4])

#loop over different values of K to get the distance
for(k in 1:k_max){
  model<-kmeans(scaled,k, nstart = 20)
  d[k]<-model$tot.withinss
}

#convert distance vector to a data frame to be plotted
v<-data.frame(vec=d)

#plot distance vs K
ggplot(data=v, aes(x=1:k_max,y = d))+ geom_point() + labs(x="K")
