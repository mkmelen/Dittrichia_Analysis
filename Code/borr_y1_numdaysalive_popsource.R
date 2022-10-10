#Mortality Surveys
#Dittrichia Project
#March 2021
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the number of days roadside and off-road plants survived.

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

data_col_NA <- data[!is.na(data$NumDaysAlive), ] # Drop NAs in only NumDaysAlive
#data_col_NA # Print data frame without NA in NumDaysAlive

#data$NumDaysAlive <- as.numeric (data$NumDaysAlive) # Convert NumDaysAlive from Int to Num

##plot Type on x-axis and NumDaysAlive on y-axis (all data)
ggplot(data=data, aes(x=Type,y=NumDaysAlive))+
  geom_boxplot(fill="light green")+
  labs(title="Survival",x="Population Type",y="Number of Days Alive")

####
#We want the mean number of days alive for each population type, save to a variable
mean.alive<-aggregate(NumDaysAlive~Type,data=data,FUN=mean)
print(mean.alive)
#Type NumDaysAlive
#1 Off-road     157.7450
#2 Roadside     155.6475

SD.alive<-aggregate(NumDaysAlive~Type,data=data,FUN=sd)
print(SD.alive)
#Type NumDaysAlive
#1 Off-road     75.13193
#2 Roadside     72.09012

SE.alive<-aggregate(NumDaysAlive~Type,data=data,FUN=se)
print(SE.alive)
#Type NumDaysAlive
#1 Off-road     3.756597
#2 Roadside     3.604506

summary.data<-merge(mean.alive,SD.alive,by="Type")
print(summary.data)
summary.data2<-merge(summary.data,SE.alive,by="Type")
print(summary.data2)

names(summary.data2) <- c("Type", "Mean", "SD", "SE")

print(summary.data2)

ggplot(summary.data2, aes(x=Type,y=Mean))+
  geom_bar(stat="identity", fill="magenta")+
  labs(title="Survival",x="Population Type",y="Number of Days Alive")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))+
  theme_bw()

