#### Survival Analysis with Random Effects ####
#Dittrichia Project
#October 2022
#Miranda Melen

#Here we will use 'coxme' which allows you to conduct mixed effects Cox proportional hazards models. Information here: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf.

?Surv #Surv creates a survival object to combine the days column (NumDaysAlive) and the censor column (Censor) to be used as a response variable in a model formula
#The model follows the same syntax as linear models (lm, lmer, etc)
#Fixed effects: "var1 * var2" will give you the interaction term and the individual variables: so "var1 + var2 + var1:var2"
#To add random effects, type "+ (1|random effect)"

####Install and Load Libraries####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

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

####Model####
dataFilter<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors = T)
head(dataFilter)#Let's look at the first 6 rows of the dataframe
str(dataFilter) #Check that each column has the right class (factor, integer, numeric, etc.)
#This dataframe has one row per plant with a calculation for how many days it took for the plant to reproduce (NumDaysAlive). The censor for this dataframe is Censor with a 1 denoting a plant lived and a 0 denoting plant died by the last census date.

##Filter model
fullmodelFilter<-coxme(Surv(NumDaysAlive,Censor)~ #Response variable: survival data
                         +Habitat * Treatment #Fixed effects and their interactions.
                       +(1 | Site)  #Random effect with random intercept only
                       +(1 | Block), #Data was blocked
                       data = dataFilter) #Dataframe

summary(fullmodelFilter)
#ERROR HERE # 
Anova(fullmodelFilter, test.statistic = "LR") #Use a type II likelihood ratio test to test your fixed effects
#It is confusing to use Anova here (the capital A is important) because this is not really an anova; R uses this function for lots of different tests of the main variables in your model.

exp(coef(fullmodelFilter)) #This number is more than 1 therefore showing that roadside pops have 28% higher probability of germination (1.28) compared to their counterpart off-road pops

exp(ranef(fullmodelFilter)$PopName) 
exp(ranef(fullmodelFilter)$'PopName/Pop')
exp(ranef(fullmodelFilter)$'PopName/Pop/DishNum')
exp(ranef(fullmodelFilter)$Rep) #Ideally these would be clustered closer; the spread is not ideal and may indicate differences in the incubator based on which shelf the trays of dishes where placed during the experiment.