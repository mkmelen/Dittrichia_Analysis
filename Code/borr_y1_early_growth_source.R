#### Early growth data by habitat of population
#Dittrichia Project
#October 2022
#Miranda Melen

#This will analyze the growth of the plants by source.

#Load libraries
library(sciplot)
library(ggplot2)
library(tidyverse)
library(dplyr)

setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

data<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv")
str(data)


