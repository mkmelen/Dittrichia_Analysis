#### Early growth data by habitat of population
#Dittrichia Project
#October 2022
#Miranda Melen

#This will analyze the growth of the plants by source.

####Install and Load Libraries####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

#Install Packages for Analysis
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("emmeans")

#Load Libraries for Analysis
library(lme4)
library(lmerTest)
library(DHARMa)
library(dplyr)
library(emmeans)

####Load Data####
mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors = T) %>% filter(Growth>0) #Here I am only looking at the Growth data that is greater than 0
#str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)

#?lmer #ReadMe for Fit Linear Mixed-Effects Models
#lmer(formula, data = NULL, REML = TRUE, control = lmerControl(), start = NULL, verbose = 0L, subset, weights, na.action, offset, contrasts = NULL, devFunOnly = FALSE)

#10 blocks, 5 treatments, 16 populations (CHE-O), 8 sites (random: population pairs; CHE), 2 habitat (fixed effect: roadside and off-road; R or O)

####Model 1####
fullmodel1<-lmer((Growth)~ #Response variable: survival data
                         Habitat * Treatment #Fixed effects and their interactions.
                       +(1 | Site) #Random effect with random intercept only
                       +(1 | Block), #Data was blocked
                       data = mydata) #Dataframe
#isSingular(fullmodel1, tol = 1e-4) #=True
#boundary (singular) fit: see help('isSingular')

####Model 2####
fullmodel2<-lmer((Growth)~ #Response variable: survival data
                   Habitat * Treatment #Fixed effects and their interactions.
                 +(1 | Block), #Data was blocked, random effect
                 data = mydata) #Dataframe

# To get p-values, run an Anova
Anova(fullmodel2)

# Look at the residuals using the DHARMa package
qqnorm(resid(fullmodel2)) #qqplot
qqline(resid(fullmodel2)) #add the line
testDispersion(fullmodel2) #red line should be in the middle of the distribution
myDHARMagraph <- simulateResiduals(fullmodel2) #making a graph using DHARMa package
plot(myDHARMagraph) #plotting graph
#QQplot deviates a bit from the line, so let's see if a transformation improves the model

#Log transform data because the qqplot is poor (https://www.statology.org/transform-data-in-r/)
mydata.Log<-log10(mydata$Growth)
hist(mydata$Growth, col='steelblue', main='Original') #Original data
shapiro.test(mydata$Growth)

hist(mydata.Log, col='coral2', main='Log Transformed') #Log transformed data
shapiro.test(mydata.Log)
#Data does not improve with log transformation... try a squareroot

#Square root transform data because the qqplot is poor 
mydata.Sqrt<-sqrt(mydata$Growth)

hist(mydata$Growth, col='steelblue', main='Original') #Original data
shapiro.test(mydata$Growth)

hist(mydata.Sqrt, col='coral2', main='Square Root Transformed') #Square root transformed data
shapiro.test(mydata.Sqrt)

####Model 3####
mydata2<-mutate(mydata,Biomass.Sqrt=sqrt(Biomass))

fullmodel3<-lmer(sqrt(Growth)~ #Response variable: survival data
                   Habitat * Treatment #Fixed effects and their interactions.
                 +(1 | Block), #Data was blocked, Random effect with random intercept only: Site is removed because it explains very little of the variance in the model
                 data = mydata2) #Dataframe

Anova(fullmodel3)

qqnorm(resid(fullmodel3)) #qqplot
qqline(resid(fullmodel3)) #add the line
testDispersion(fullmodel3) #red line should be in the middle of the distribution
myDHARMagraph3 <- simulateResiduals(fullmodel3) #making a graph using DHARMa package, also testing for heteroscedasticity ->https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#heteroscedasticity
plot(myDHARMagraph3) #plotting graph

#?emmeans, emmeans(model, pairwise ~ treatment)
emmeans(fullmodel3, pairwise ~ Treatment)

####Plotting Successful Model###
#ggplot(data=mydata,aes(x=Treatment,y=Growth))+geom_boxplot() #plot data from original data

ggplot(data=mydata,aes(x=Treatment,y=sqrt(Growth)))+geom_boxplot() #plot data from sqrt(data)
