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
  geom_bar(stat="identity", fill="magenta")+
  labs(title="Survival",x="Population Type",y="Number of Days Alive")
#but not quite done yet...

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

#Now merge the tables into one
summary.data<-merge(mean.alive,SD.alive,by="Type")
print(summary.data)
summary.data2<-merge(summary.data,SE.alive,by="Type")
print(summary.data2)

#But the names are screwed up, so let's rename here
names(summary.data2) <- c("Type", "Mean", "SD", "SE")

print(summary.data2) #That's better

#Now, make me a bar graph with the correct error bars!
ggplot(summary.data2, aes(x=Type,y=Mean))+
  geom_bar(stat="identity", fill="magenta")+
  labs(title="Survival",x="Population Type",y="Number of Days Alive")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))+
  theme_bw()

#H0: There is no difference in survival between roadside and off-road populations.
#HA: There is a difference in survival between roadside and off-road populations.

t.test(NumDaysAlive~Type,data=data,alternative="two.sided") #a two-tailed t-test

#Welch Two Sample t-test

#data:  NumDaysAlive by Type
#t = 0.40289, df = 796.64, p-value = 0.6871
#alternative hypothesis: true difference in means between group Off-road and group Roadside is not equal to 0
#95 percent confidence interval:
#  -8.121984 12.316984
#sample estimates:
#  mean in group Off-road mean in group Roadside 
#157.7450               155.6475 
