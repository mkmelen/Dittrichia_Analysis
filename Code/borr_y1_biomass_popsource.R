#Mortality Surveys
#Dittrichia Project
#October 2022
#Blue Oak Ranch Reserve, Competition Experiment

#This code will evaluate the biomass of surviving plants by source.

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

##plot Source on x-axis and Biomass on y-axis (all data)
ggplot(data=data, aes(x=Source,y=Biomass))+
  geom_bar(stat="identity", fill="turquoise")+
  labs(title="Survival",x="Population Source",y="Biomass (g)")
#but not quite done yet...

#We want the mean number of days alive for each population type, save to a variable
mean.biomass<-aggregate(Biomass~Source,data=data,FUN=mean)
print(mean.biomass)
#Source  Biomass
#1   BAY-O 2.513333
#2   BAY-R 2.089862
#3   CHE-O 1.995806
#4   CHE-R 2.648444
#5   GUA-O 2.349050
#6   GUA-R 3.183138
#7   LEX-O 2.060759
#8   LEX-R 2.804323
#9   OAP-O 2.465063
#10  OAP-R 2.681458
#11  PAR-O 3.233750
#12  PAR-R 2.173107
#13  PEN-O 3.197120
#14  PEN-R 3.392643
#15  SSJ-O 2.540188
#16  SSJ-R 4.430304

SD.biomass<-aggregate(Biomass~Source,data=data,FUN=sd)
print(SD.biomass)
#Source Biomass
#1   BAY-O  4.200217
#2   BAY-R  3.765434
#3   CHE-O  4.057640
#4   CHE-R  4.338742
#5   GUA-O  3.670942
#6   GUA-R  5.873859
#7   LEX-O  3.182763
#8   LEX-R  6.636480
#9   OAP-O  4.549572
#10  OAP-R  4.980247
#11  PAR-O  7.281640
#12  PAR-R  5.019653
#13  PEN-O  8.052727
#14  PEN-R  7.614996
#15  SSJ-O  4.536587
#16  SSJ-R 12.045873

SE.biomass<-aggregate(Biomass~Source,data=data,FUN=se)
print(SE.biomass)
#Source Biomass
#1   BAY-O 0.8083322
#2   BAY-R 0.6992235
#3   CHE-O 0.7287737
#4   CHE-R 0.8349914
#5   GUA-O 0.8208476
#6   GUA-R 1.0907483
#7   LEX-O 0.5910242
#8   LEX-R 1.1919470
#9   OAP-O 0.8042584
#10  OAP-R 1.0165886
#11  PAR-O 1.4863586
#12  PAR-R 0.9486253
#13  PEN-O 1.6105454
#14  PEN-R 1.4390990
#15  SSJ-O 0.8019629
#16  SSJ-R 2.5117381

#Now merge the tables into one
summary.data<-merge(mean.biomass,SD.biomass,by="Source")
print(summary.data)
summary.data2<-merge(summary.data,SE.biomass,by="Source")
print(summary.data2)

#But the names are screwed up, so let's rename here
names(summary.data2) <- c("Source", "Mean", "SD", "SE")

print(summary.data2) #That's better

#Now, make me a bar graph with the correct error bars!
ggplot(summary.data2, aes(x=Source,y=Mean))+
  geom_bar(stat="identity", fill="turquoise")+
  labs(title="Biomass",x="Population Source",y="Biomass (g)")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))+
  theme_bw()

#### Missing stats here... not sure how to do the correct type of ANOVA because I really want to pair the sources by roadside and off-road.