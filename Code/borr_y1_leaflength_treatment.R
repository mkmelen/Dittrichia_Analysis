#Longest leaf measurements by treatment
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
#$ Population    : chr  "GUA-O" "SSJ-O" "PEN-R" "PAR-O" ...
#$ Site          : chr  "GUA" "SSJ" "PEN" "PAR" ...
#$ Habitat       : chr  "Off-road" "Off-road" "Roadside" "Off-road" ...
#$ Treatment     : chr  "Raking" "Raking" "Raking" "Raking" ...
#$ PlantDate     : chr  "2/27/21" "2/27/21" "2/27/21" "2/27/21" ...
#$ LeafMeas1     : num  18.75 15.71 9.13 16.39 12.85 ...
#$ LeafMeas2     : int  24 31 19 9 14 16 19 27 22 22 ...
#$ Growth        : num  5.25 15.29 9.87 -7.39 1.15 ...

#data_col_NA <- data[!is.na(data$LeafMeas2), ] # Drop NAs in only LeafMeas2
#data_col_NA # Print data frame without NA in LeafMeas2

data$LeafMeas2 <- as.numeric (data$LeafMeas2) # Convert LeafMeas2 from Char to Num
#which(is.na(data$LeafMeas2))

##plot Treatment on x-axis and Longest Leaf Length (mm) (LeafMeas2) on y-axis (all data)
ggplot(data=data, aes(x=Treatment,y=LeafMeas2))+
  geom_boxplot(fill="light green")+
  labs(title="Leaf Length by Treatment Type",x="Treatment",y="Longest Leaf Length (mm)")

ggplot(data=data, aes(x=Treatment,y=LeafMeas2))+
  labs(title="Mean Leaf Length by Treatment Type",x="Treatment",y="Mean Longest Leaf Length (mm)")+
  geom_jitter(alpha=0.5,height=0,show.legend = F)+ #adds all data, spread out 
  geom_boxplot(alpha = 0.3,show.legend = F,outlier.shape = NA,width=0.5)#if you use this, comment out the geom_boxplot earlier in the code; Alpha 1=100%, 0.3= 30% fade

#We want the mean length of leaf for each treatment, save to a variable
mean.leaf.length<-aggregate(LeafMeas2~Treatment,data=data,FUN=mean)
print(mean.leaf.length)
#Treatment LeafMeas2
#1   Biomass Removal  46.81333
#2           Control  21.46853
#3         Hemizonia  21.29197
#4            Raking  22.45033
#5 Raking + Clipping  26.64626

#Calculating SD
SD.leaf.length<-aggregate(LeafMeas2~Treatment,data=data,FUN=sd)
colnames(SD.leaf.length)<-c("Treatment","sd")
print(SD.leaf.length)
#Treatment sd
#1   Biomass Removal 11.074723
#2           Control  8.797250
#3         Hemizonia  8.232076
#4            Raking  9.959711
#5 Raking + Clipping  9.753681

summary.data<-merge(mean.leaf.length,SD.leaf.length,by="Treatment")
print(summary.data)

#Plotting with SD
ggplot(data=summary.data, aes(x=Treatment,y=LeafMeas2))+
  geom_point(fill="light green")+
  labs(title="Mean Leaf Length by Treatment Type",x="Treatment",y="Mean Longest Leaf Length (mm)")+
  geom_errorbar(aes(ymin=LeafMeas2-sd, ymax=LeafMeas2+sd), width=.2,position=position_dodge(.9)) 

#Calculating SE
SE.leaf.length<-aggregate(LeafMeas2~Treatment,data=data,FUN=se)
colnames(SE.leaf.length)<-c("Treatment","se")
print(SE.leaf.length)
#Treatment LeafMeas2
#1   Biomass Removal 0.9042473
#2           Control 0.7356630
#3         Hemizonia 0.7033137
#4            Raking 0.8105098
#5 Raking + Clipping 0.8044700

summary.data2<-merge(summary.data,SE.leaf.length,by="Treatment")
print(summary.data2)

#Plotting with SE
ggplot(data=summary.data, aes(x=Treatment,y=LeafMeas2))+
  geom_point(fill="light green")+
  labs(title="Mean Leaf Length by Treatment Type",x="Treatment",y="Mean Longest Leaf Length (mm)")+
  geom_errorbar(aes(ymin=LeafMeas2-se, ymax=LeafMeas2+se), width=.2,position=position_dodge(.9)) 


