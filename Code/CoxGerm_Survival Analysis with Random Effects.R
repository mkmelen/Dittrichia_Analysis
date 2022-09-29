#### Survival Analysis with Random Effects ####
#Dittrichia Project
#August 2021
#Miranda Melen, Michael Fernandez, and Nicky Lustenhouwer

#Here we will use 'coxme' which allows you to conduct mixed effects Cox proportional hazards models. Information here: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf. We conducted two germination experiments using Dittrichia graveolens seeds on filter paper (experiment 1: CoxGermFilter8-2-21.csv) and seeds on two types of soil: construction fill and forest soil (experiment 2: CoxGermSoil8-2-21.csv).

?Surv #Surv creates a survival object to combine the days column (DaysToGerm) and the censor column (CensorR) to be used as a response variable in a model formula
        #The model follows the same syntax as linear models (lm, lmer, etc)
        #Fixed effects: "var1 * var2" will give you the interaction term and the individual variables: so "var1 + var2 + var1:var2"
        #To add random effects, type "+ (1|random effect)"

####Install and Load Libraries####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Analysis") 

#Install Packages for Analysis
install.packages("coxme")
install.packages("car")
install.packages("survival")
install.packages("multcomp")

#Load Libraries for Analysis
library(coxme)
library(car)
library(survival)
library(multcomp)

#Install Packages for Graphing
install.packages("tidyr")
install.packages("ggfortify")
install.packages("RColorBrewer")
install.packages("survMisc")
install.packages("ggplot2")

#Load Libraries for Graphing
library(tidyr)
library(ggfortify)
library(RColorBrewer)
library(survMisc)
library(ggplot2)

####Experiment 1: Filter Paper####
dataFilter <- read.csv("CoxGermFilter8-2-21.csv", stringsAsFactors = T)
head(dataFilter) #Let's look at the first 6 rows of the dataframe
str(dataFilter) #Check that each column has the right class (factor, integer, numeric, etc.)
dataFilter$DishNum <- as.factor(dataFilter$DishNum) #We need to change DishNum to be a factor rather than an int
str(dataFilter) #Make sure DishNum is now a factor with 80 levels (= 80 dishes)

#This dataframe has one row per seed (800 lines) with a calculation for how many days it took for the seed to germinate (DaysToGerm). The censor for this dataframe is CensorR with a 1 denoting a seed germinated and a 0 denoting when a seed didn't germinate by the last census date (Census = 8/2/21).

##Filter model
fullmodelFilter <- coxme(Surv(DaysToGerm,CensorR) ~ #Response variable: survival data
                                 PopType + #Fixed effects and their interactions
                                 (1 | PopName/Pop/DishNum) + #Random effect with random intercept only
                                 (1 | Rep), #Rep is a block (all the dishes were grouped together by Rep)
                         #(1 + PopType | PopName/Pop/DishNum), # Random effect with random intercept and random slope <- could try this, but may not be best option for this data set
                         data = dataFilter) #Dataframe

summary(fullmodelFilter)
Anova(fullmodelFilter, test.statistic = "LR") #Use a type II likelihood ratio test to test your fixed effects
#It is confusing to use Anova here (the capital A is important) because this is not really an anova; R uses this function for lots of different tests of the main variables in your model.

exp(coef(fullmodelFilter)) #This number is more than 1 therefore showing that roadside pops have 28% higher probability of germination (1.28) compared to their counterpart off-road pops

exp(ranef(fullmodelFilter)$PopName) 
exp(ranef(fullmodelFilter)$'PopName/Pop')
exp(ranef(fullmodelFilter)$'PopName/Pop/DishNum')
exp(ranef(fullmodelFilter)$Rep) #Ideally these would be clustered closer; the spread is not ideal and may indicate differences in the incubator based on which shelf the trays of dishes where placed during the experiment.

####Experiment 2: Construction Fill and Forest Soils####
dataSoil <- read.csv("CoxGermSoil8-2-21.csv", stringsAsFactors = T)
head(dataSoil) # Let's look at the first 6 rows of the dataframe
str(dataSoil) #Check that each column has the right class (factor, integer, numeric, etc.)
dataSoil$DishNum <- as.factor(dataSoil$DishNum)

#This dataframe has one row per seed (1600 lines) with a calculation for how many days it took for the seed to germinate (DaysToGerm). The censor for this dataframe is CensorR with a 1 denoting a seed germinated and a 0 denoting when a seed didn't germinate by the last census date (Census = 8/2/21).

##Soil model
fullmodelSoil <- coxme(Surv(DaysToGerm,CensorR) ~ #Response variable: survival data
                         PopType*DishType + #Fixed effects and their interactions
                         (1 | PopName/Pop/DishNum), #Random effects
                 dataSoil) #Dataframe

summary(fullmodelSoil)
Anova(fullmodelSoil, test.statistic = "LR") #Use a type II likelihood ratio test to test your fixed effects
#I think this shows that the main effects and the interaction term are not significant. Do you agree?