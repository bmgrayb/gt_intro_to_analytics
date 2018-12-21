rm(list=ls())
set.seed(1)
library(ggplot2)
library(plotly)

#read in data
security_data<-read.table("data/airport_security_data.csv", header = FALSE, sep = ",")

colnames(security_data)<-c("Checkers","Scanners", "MeanWaitTime", "MedianWaitTime","Throughput")

#plot number of scanners vs checkers and whether it was under 15 mins
ggplot(security_data, aes(Scanners, Checkers)) + 
  geom_point(aes(colour = cut(MeanWaitTime, c(-Inf, 15,16, Inf)))) +
  scale_color_manual(name = "Mean Wait Time",
                     values = c("(-Inf,15]" = "red","(16, Inf]" = "black"),
                     labels = c("<= 15 min", "> 15 min"))

#heat maps
plot_ly(security_data, x = ~Checkers, y = ~Scanners, z = ~MeanWaitTime, colorscale="Greys", type = "heatmap")
plot_ly(security_data, x = ~Checkers, y = ~Scanners, z = ~MedianWaitTime, colorscale="Greys", type = "heatmap")
plot_ly(security_data, x = ~Checkers, y = ~Scanners, z = ~Throughput, colorscale="Greys", type = "heatmap")
