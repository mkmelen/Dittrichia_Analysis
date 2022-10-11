#Mortality Surveys
#Dittrichia Project
#October 2022
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the longest leaf length of the data from the Blue Oak competition experiment.

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

setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

data<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv")
str(data)
#$ Block         : chr  "A" "A" "A" "A" ...
#$ Plot          : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Flag          : chr  "A1" "A1" "A1" "A1" ...
#$ Pos           : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Flag_Pos      : chr  "A1_01" "A1_02" "A1_03" "A1_04" ...
#$ Source        : chr  "GUA-O" "SSJ-O" "PEN-R" "PAR-O" ...
#$ Pop           : chr  "GUA" "SSJ" "PEN" "PAR" ...
#$ Type          : chr  "Off-road" "Off-road" "Roadside" "Off-road" ...
#$ Trt           : chr  "Raking" "Raking" "Raking" "Raking" ...
#$ PlantDate     : chr  "2/27/2021" "2/27/2021" "2/27/2021" "2/27/2021" ...
#$ LeafMeas1     : num  18.75 15.71 9.13 16.39 12.85 ...
#$ LeafMeas2     : chr  "24" "31" "19" "9" ...

data_col_NA <- data[!is.na(data$LeafMeas2), ] # Drop NAs in only LeafMeas2
data_col_NA # Print data frame without NA in LeafMeas2

data$LeafMeas2 <- as.numeric (data$LeafMeas2) # Convert LeafMeas2 from Char to Num
which(is.na(data$LeafMeas2))

##plot Treatment on x-axis and Longest Leaf Length (mm) (LeafMeas2) on y-axis (all data)
ggplot(data=data, aes(x=Trt,y=LeafMeas2))+
  geom_boxplot(fill="light green")+
  labs(title="Leaf Length by Treatment Type",x="Treatment",y="Longest Leaf Length (mm)")

#We want the mean length of leaf for each treatment, save to a variable
mean.leaf.length<-aggregate(LeafMeas2~Trt,data=data,FUN=mean)
print(mean.leaf.length)
#LeafMeas2
#1   Control                              19.15925
#2      HECO                              19.22175
#3      High                              18.75669
#4       Low                              17.97350
#5    Medium                              17.96525

SD.leaf.length<-aggregate(LeafMeas2~Trt,data=data,FUN=sd)
print(SD.leaf.length)
#Treatment LeafMeas2
#1   Control                              5.129316
#2      HECO                              5.251399
#3      High                              5.397813
#4       Low                              5.100468
#5    Medium                              5.382011

SE.leaf.length<-aggregate(LeafMeas2~Trt,data=data,FUN=se)
print(SE.leaf.length)
#Treatment LeafMeas2
#1   Control                             0.4055080
#2      HECO                             0.4151596
#3      High                             0.4267346
#4       Low                             0.4032274
#5    Medium                             0.4254853

summary.data<-merge(mean.leaf.length,SD.leaf.length,by="Trt")
print(summary.data)
summary.data2<-merge(summary.data,SE.leaf.length,by="Trt")
print(summary.data2)

ggplot(data=mean.leaf.length, aes(x=Trt,y=LeafMeas2))+
  geom_point(fill="light green")+
  labs(title="Mean Leaf Length by Treatment Type",x="Treatment",y="Mean Longest Leaf Length (mm)")+
  geom_errorbar(aes(ymin=LeafMeas2-sd, ymax=LeafMeas2+sd), width=.2,position=position_dodge(.9)) 
# error in LeafMeas2-sd: non-numeric argument to binary operator
