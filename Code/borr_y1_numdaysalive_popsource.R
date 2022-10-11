#Mortality Surveys
#Dittrichia Project
#March 2021
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the number of days plants survived by source.

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

##plot Source on x-axis and NumDaysAlive on y-axis (all data)
ggplot(data=data, aes(x=Source,y=NumDaysAlive))+
  geom_bar(stat="identity", fill="magenta")+
  labs(title="Survival",x="Population Source",y="Number of Days Alive")
#but not quite done yet...

#We want the mean number of days alive for each population type, save to a variable
mean.alive<-aggregate(NumDaysAlive~Source,data=data,FUN=mean)
print(mean.alive)
#Source NumDaysAlive
#1   BAY-O       154.12
#2   BAY-R       162.62
#3   CHE-O       169.42
#4   CHE-R       167.70
#5   GUA-O       133.22
#6   GUA-R       160.24
#7   LEX-O       154.22
#8   LEX-R       156.88
#9   OAP-O       172.70
#10  OAP-R       148.28
#11  PAR-O       153.96
#12  PAR-R       160.00
#13  PEN-O       146.16
#14  PEN-R       153.18
#15  SSJ-O       178.16
#16  SSJ-R       136.28

SD.alive<-aggregate(NumDaysAlive~Source,data=data,FUN=sd)
print(SD.alive)
#Source NumDaysAlive
#1   BAY-O     75.42387
#2   BAY-R     75.13767
#3   CHE-O     73.92470
#4   CHE-R     73.71685
#5   GUA-O     66.51325
#6   GUA-R     69.99346
#7   LEX-O     77.85237
#8   LEX-R     70.69123
#9   OAP-O     78.31575
#10  OAP-R     73.25216
#11  PAR-O     74.43528
#12  PAR-R     71.87489
#13  PEN-O     74.11336
#14  PEN-R     75.19436
#15  SSJ-O     74.66916
#16  SSJ-R     66.86906

SE.alive<-aggregate(NumDaysAlive~Source,data=data,FUN=se)
print(SE.alive)
#Source NumDaysAlive
#1   BAY-O    10.666547
#2   BAY-R    10.626071
#3   CHE-O    10.454531
#4   CHE-R    10.425136
#5   GUA-O     9.406393
#6   GUA-R     9.898570
#7   LEX-O    11.009988
#8   LEX-R     9.997250
#9   OAP-O    11.075520
#10  OAP-R    10.359420
#11  PAR-O    10.526738
#12  PAR-R    10.164645
#13  PEN-O    10.481212
#14  PEN-R    10.634088
#15  SSJ-O    10.559814
#16  SSJ-R     9.456713

#Now merge the tables into one
summary.data<-merge(mean.alive,SD.alive,by="Source")
print(summary.data)
summary.data2<-merge(summary.data,SE.alive,by="Source")
print(summary.data2)

#But the names are screwed up, so let's rename here
names(summary.data2) <- c("Source", "Mean", "SD", "SE")

print(summary.data2) #That's better

#Now, make me a bar graph with the correct error bars!
ggplot(summary.data2, aes(x=Source,y=Mean))+
  geom_bar(stat="identity", fill="magenta")+
  labs(title="Survival",x="Population Source",y="Number of Days Alive")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))+
  theme_bw()

#### Missing stats here... not sure how to do the correct type of ANOVA because I really want to pair the sources by roadside and off-road.