#Mortality Surveys
#Dittrichia Project
#October 2022
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the biomass in roadside and off-road plants that survived.

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

data_col_NA <- data[!is.na(data$Biomass), ] # Drop NAs in only Biomass
#data_col_NA # Print data frame without NA in Biomass

##plot Type on x-axis and Biomass on y-axis (all data)
ggplot(data=data, aes(x=Type,y=Biomass))+
  geom_bar(stat="identity", fill="turquoise")+
  labs(title="Biomass",x="Population Type",y="Biomass (g)")
#but not quite done yet...

#We want the mean biomass for each population type, save to a variable
mean.biomass<-aggregate(Biomass~Type,data=data,FUN=mean)
print(mean.biomass)
#Type Biomass
#1 Off-road 2.518995
#2 Roadside 2.892475

SD.biomass<-aggregate(Biomass~Type,data=data,FUN=sd)
print(SD.biomass)
#Type Biomass
#1 Off-road 5.050793
#2 Roadside 6.525340

SE.biomass<-aggregate(Biomass~Type,data=data,FUN=se)
print(SE.biomass)
#Type Biomass
#1 Off-road 0.3405244
#2 Roadside 0.4409416

#Now merge the tables into one
summary.data<-merge(mean.biomass,SD.biomass,by="Type")
print(summary.data)
summary.data2<-merge(summary.data,SE.biomass,by="Type")
print(summary.data2)

#But the names are screwed up, so let's rename here
names(summary.data2) <- c("Type", "Mean", "SD", "SE")

print(summary.data2) #That's better

#Now, make me a bar graph with the correct error bars!
ggplot(summary.data2, aes(x=Type,y=Mean))+
  geom_bar(stat="identity", fill="turquoise")+
  labs(title="Biomass",x="Population Type",y="Biomass (g)")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))+
  theme_bw()

#H0: There is no difference in survival between roadside and off-road populations.
#HA: There is a difference in survival between roadside and off-road populations.

t.test(Biomass~Type,data=data,alternative="two.sided") #a two-tailed t-test

#Welch Two Sample t-test

#data:  Biomass by Type
#t = -0.67037, df = 410.3, p-value = 0.503
#alternative hypothesis: true difference in means between group Off-road and group Roadside is not equal to 0
#95 percent confidence interval:
#  -1.4686516  0.7216928
#sample estimates:
#  mean in group Off-road mean in group Roadside 
#2.518995               2.892475 