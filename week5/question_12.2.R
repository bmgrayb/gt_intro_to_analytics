rm(list=ls())
set.seed(1)
library(FrF2)

#list out some features we would like to show in houses
features<-c("Large Yard", "Solar Panels", "Fence", "Attached Garage","Updated Appliances",
            "Energy Efficient","On-Suite Bathroom","Finished Basement","Exclusive Neighborhood","Wood Floors")

#easier to read than 1 and -1
lvls<-c("yes","no")

#store results of fractional factorial
exp<-FrF2(16,factor.names = features, default.levels = lvls)

#vector to keep track of all the "yes" properties listed in the experiments above
house_properties<-rep("",nrow(exp))

#loop over all rows, for each yes, add column name to vector
#each row represents a house with the specific features to show
for(i in 1:nrow(exp)){
  
  curr<-""
  
  for(j in 1:ncol(exp)){
    
    if(exp[i,j] == "yes"){
      curr<-paste(curr,colnames(exp)[j],sep = " ")
    }
    
  }
  
  if(curr==""){
    curr<-"None of the above"
  }
  
  house_properties[i]<-curr
  
}

house_properties
