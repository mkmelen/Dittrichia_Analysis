#Mortality Surveys
#Dittrichia Project
#March 2021
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the mortality rates of the data from the Blue Oak competition experiment.

#There are 5 treatments.
#Control = control plots: no manipulation of the plot
#Low = low disturbance to the plots: removing thatch prior to planting
#Medium = medium disturbance to the plots: removing thatch prior to planting and clipping of vegetation during growing season to decrease competition
#High = high disturbance to the plots: biomass was removed from plots and everything was hoed prior to planting
#HECO = Hemizonia congesta subsp. luzulifola seed was added to plots prior to planting, no other manipulation of the plots

#Load libraries
library(sciplot)
library(ggplot2)
library(tidyverse)

setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Analysis") 

dat<-read.csv("Blue Oak Competition - Mortality and Size.csv")
str(dat)
#$ Block                                : chr  "A" "A" "A" "A" ...
#$ Plot                                 : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Flag                                 : chr  "A1" "A1" "A1" "A1" ...
#$ Position                             : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Source                               : chr  "GUA-O" "SSJ-O" "PEN-R" "PAR-O" ...
#$ Treatment                            : chr  "Low" "Low" "Low" "Low" ...
#$ Longest.Leaf.Length.replant.mm..final: num  18.75 15.71 9.13 16.39 12.85 ...
#$ Transplanting.date.Final             : chr  "2/27/21" "2/27/21" "2/27/21" "2/27/21" ...

#plot Treatment on x-axis and Longest Leaf Length (mm) on y-axis (all data)
ggplot(data=dat, aes(x=Treatment,y=Longest.Leaf.Length.replant.mm..final))+
  geom_boxplot(fill="light green")+
  labs(title="Leaf Length by Treatment Type",x="Treatment",y="Longest Leaf Length (mm)")

#We want the mean length of leaf for each treatment, save to a variable
mean.leaf.length<-aggregate(Longest.Leaf.Length.replant.mm..final~Treatment,data=dat,FUN=mean)
print(mean.leaf.length)
#Treatment Longest.Leaf.Length.replant.mm..final
#1   Control                              19.15925
#2      HECO                              19.22175
#3      High                              18.75669
#4       Low                              17.97350
#5    Medium                              17.96525

SD.leaf.length<-aggregate(Longest.Leaf.Length.replant.mm..final~Treatment,data=dat,FUN=sd)
print(SD.leaf.length)
#Treatment Longest.Leaf.Length.replant.mm..final
#1   Control                              5.129316
#2      HECO                              5.251399
#3      High                              5.397813
#4       Low                              5.100468
#5    Medium                              5.382011

SE.leaf.length<-aggregate(Longest.Leaf.Length.replant.mm..final~Treatment,data=dat,FUN=se)
print(SE.leaf.length)
#Treatment Longest.Leaf.Length.replant.mm..final
#1   Control                             0.4055080
#2      HECO                             0.4151596
#3      High                             0.4267346
#4       Low                             0.4032274
#5    Medium                             0.4254853

summary.dat<-merge(mean.leaf.length,SD.leaf.length,by="Treatment")
print(summary.dat)
summary.dat2<-merge(summary.dat,SE.leaf.length,by="Treatment")
print(summary.dat2)

ggplot(data=mean.leaf.length, aes(x=Treatment,y=Longest.Leaf.Length.replant.mm..final))+
  geom_point(fill="light green")+
  labs(title="Mean Leaf Length by Treatment Type",x="Treatment",y="Mean Longest Leaf Length (mm)")+
  geom_errorbar(aes(ymin=Longest.Leaf.Length.replant.mm..final-sd, ymax=Longest.Leaf.Length.replant.mm..final+sd), width=.2,position=position_dodge(.9)) 

