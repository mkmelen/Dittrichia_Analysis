#### Survival Analysis with Random Effects ####
#Dittrichia Project
#October 2022
#Miranda Melen

#Here we will use 'coxme' which allows you to conduct mixed effects Cox proportional hazards models. Information here: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf.

#?Surv #Surv creates a survival object to combine the days column (NumDaysAlive) and the censor column (Censor) to be used as a response variable in a model formula
#The model follows the same syntax as linear models (lm, lmer, etc)
#Fixed effects: "var1 * var2" will give you the interaction term and the individual variables: so "var1 + var2 + var1:var2"
#To add random effects, type "+ (1|random effect)"

####Install Libraries####
#install.packages("coxme")
#install.packages("car")
#install.packages("survival")
#install.packages("multcomp")
#install.packages("tidyr")
#install.packages("ggfortify")
#install.packages("RColorBrewer")
#install.packages("survMisc")
#install.packages("tidyverse")

####Load Libraries####
library(coxme)
library(car)
library(survival)
library(multcomp)
library(tidyr)
library(ggfortify)
library(RColorBrewer)
library(survMisc)
library(tidyverse)

####Load Data####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors=T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
#str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)
mydata$Site<-as.character(mydata$Site)

#This dataframe has one row per plant with a calculation for how many days it took for the plant to reproduce (NumDaysAlive). The censor for this dataframe is Censor with a 1 denoting a plant lived and a 0 denoting plant died by the last census date.

####Histograms####
#Original data
hist(mydata$NumDaysAlive,col='steelblue',main='Original') #Original data is skewed, let's test for normality and consider log transforming the data
shapiro.test(mydata$NumDaysAlive)
#Shapiro-Wilk normality test
#data:  mydata$NumDaysAlive
#W = 0.81823, p-value < 2.2e-16

#THE PLAN: explore the data and figure out if the bimodal distribution of the data is driven by a treatment... so make some exploratory graphs
#Bad code
#ggplot(mydata, aes(x=NumDaysAlive,y=Coumt,group=Treatment)) +
  #geom_line(stat="identity", position=position_dodge()), width=.2)

####Model 1####
fullmodel1<-coxme(Surv(NumDaysAlive,Censor)~Habitat*Treatment+(1|Site)+(1|Block),data=mydata)
summary(fullmodel1)
#ERROR HERE # Error in update.default(formula(object), formula.) : need an object with call component
anova(fullmodel1, test.statistic = "LR") #Use a type II likelihood ratio test to test your fixed effects
#It is confusing to use Anova here (the capital A is important) because this is not really an anova; R uses this function for lots of different tests of the main variables in your model.

exp(coef(fullmodel1)) #This extracts fixed effects?
#HabitatRoadside                           TreatmentControl 
#0.9915997                                  1.0927160 
#TreatmentHemizonia                            TreatmentRaking 
#1.0675844                                  1.1032617 
#TreatmentRaking + Clipping           HabitatRoadside:TreatmentControl 
#0.9045366                                  0.9068636 
#HabitatRoadside:TreatmentHemizonia            HabitatRoadside:TreatmentRaking 
#1.1862529                                  0.9924184 
#HabitatRoadside:TreatmentRaking + Clipping 
#1.0801331 

exp(ranef(fullmodel1)$Population)
exp(ranef(fullmodel1)$'Population/Pop')
exp(ranef(fullmodel1)$Rep) #Ideally these would be clustered closer; the spread is not ideal and may indicate differences in the incubator based on which shelf the trays of dishes where placed during the experiment.