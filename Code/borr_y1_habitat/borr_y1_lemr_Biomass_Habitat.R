#### Linear Mixed-Effects Models - Biomass and Habitat ####
#Dittrichia Project
#October 2022
#Miranda Melen

#This code uses Biomass data with Habitat (roadside and off-road) and Treatment in a lmer model. Anova and Tukey tests are used on the successful Model3 with the creation of a box plot as a finished product.

#Note: 10 blocks, 5 treatments, 16 populations (CHE-O), 8 sites (random: population pairs; CHE), 2 habitat (fixed effect: roadside and off-road; R or O)

####Install Libraries####
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("DHARMa")
#install.packages("dplyr")
#install.packages("emmeans")
#install.packages("AICcmodavg")
#install.packages("ggplot2")

####Load Libraries####
library(lme4)
library(lmerTest)
library(DHARMa)
library(dplyr)
library(emmeans)
library(AICcmodavg)
library(ggplot2)

####Load Data####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors=T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
#str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)
mydata$Site<-as.character(mydata$Site)

####Histograms####
#Original data
hist(mydata$Biomass,col='steelblue',main='Original') #Original data is skewed, let's test for normality and consider log transforming the data
shapiro.test(mydata$Biomass)
#Shapiro-Wilk normality test
#data:  mydata$Biomass
#W = 0.50901, p-value < 2.2e-16

#Log transform data (https://www.statology.org/transform-data-in-r/)
mydata.Log<-log10(mydata$Biomass)
hist(mydata.Log,col='coral2',main='Log Transformed') #Log transformed data, this looks better than the original distribution
shapiro.test(mydata.Log)
#Shapiro-Wilk normality test
#data:  mydata.Log
#W = 0.95322, p-value = 1.428e-10

#Log transformed data has a better distribution than the original data so we will use the log transformed data with our models

####Model 1####
#?lmer #ReadMe for Fit Linear Mixed-Effects Models
#lmer(formula,data=NULL,REML=TRUE,control =lmerControl(),start=NULL,verbose=0L,subset,weights,na.action,offset,contrasts=NULL,devFunOnly=FALSE)

#fullmodel1<-lmer(log(Biomass)~ #Response variable: biomass
#                   Habitat*Treatment #Fixed effects and their interactions(*)
#                 +(1|Site)+(1|Block), #Random effect with random intercept only
#                 data=mydata) #Dataframe

fullmodel1<-lmer(log(Biomass)~Habitat*Treatment+(1|Site)+(1|Block),data=mydata)
isSingular(fullmodel1,tol=1e-4) #=True
#boundary (singular) fit: see help('isSingular')
summary(fullmodel1) #Variance explained by Site = 0.000
anova(fullmodel1)
#Site as a random effect does not explain any of the variance in the model, therefore let's try Site as a fixed effect to demonstrate that it doesn't add to the model.

####Model 2####
#Modeling Site as a fixed  effect
fullmodel2<-lmer(log(Biomass)~Habitat*Treatment+Site+(1|Block),data=mydata)
summary(fullmodel2)
anova(fullmodel2)
#Site as a fixed effect is not significant, therefore it should not be used as a fixed effect in addition to it not being used as a random effect.

####Model 3####
#Site is removed from this model because it explains very little of the variance
fullmodel3<-lmer(log(Biomass)~Habitat*Treatment+(1|Block),data=mydata)
summary(fullmodel3)
Anova(fullmodel3)

#Now we'll look at the QQ plots and the residuals using DHARMa
qqnorm(resid(fullmodel3)) #qqplot
qqline(resid(fullmodel3)) #add the line
testDispersion(fullmodel3) #red line should be in the middle of the distribution
myDHARMagraph3<-simulateResiduals(fullmodel3) #making a graph using DHARMa package, also testing for heteroscedasticity, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#heteroscedasticity
plot(myDHARMagraph3) #plotting graph. At this point, you don't want any text or lines to be red.

#When we compare the model summaries and Anova of Models 2 & 3, we see that the removal of Site doesn't impact the model. Therefore the more simple Model 3 is a better choice. But let's keep checking...

###Compare AICs####
#Now I need to compare the AIC scores for all the models to tell me which is the better model (but it will not say which fits my data better, that is why I did all the DHARMa stuff)

#Using aictab to make the comparison of models and table 
models<-list(fullmodel1,fullmodel2,fullmodel3)
mod.names<-c('Site.Random','Site.Fixed','No.Site')
aictab(cand.set=models,modnames=mod.names)

#Model selection based on AICc:
#            K     AICc      Delta_AICc AICcWt Cum.Wt  Res.LL
#No.Site     12 1391.09       0.00   0.74   0.74 -683.18
#Site.Random 13 1393.21       2.12   0.26   1.00 -683.18
#Site.Fixed  19 1412.14      21.05   0.00   1.00 -686.16
#The lowest AICc score is listed first and indicates the best fitting model, here, Model 3 (No.Site) where Site is not included. The cut-off for comparing Models is 2 units. The difference between Model 1 (Site.Random) and Model 3 (No.Site) is 2.12. So Model 3 is marginally better than Model 2.

#Other ideas that were explored to keep Site but resulted in singular error and Site explaining 0.000 of the variance: 
#fullmodel4<-lmer(log(Biomass)~ Treatment + (1|Site) + (1|Block), data = mydata)
#summary(fullmodel4)
#fullmodel5<-lmer(log(Biomass)~ Treatment + Habitat + (1|Site) + (1|Block), data = mydata)
#summary(fullmodel5)

####Post-Hoc Test####
#Nicky says: you should remove non-significant interaction terms. This is what we discussed last time with Ingrid - it is difficult to judge the main effects (Habitat and Site) when you also have the interaction term in there, so when it is not significant, just fit a new model without it. This may also be a way out of your convergence errors by the way. You could try putting both random effects back in but removing Habitat: so ~ Treatment + (1|Site) + (1|Block)

fullmodel3.1<-lmer(log(Biomass)~Treatment+(1|Block),data=mydata)
summary(fullmodel3.1)
anova(fullmodel3.1)
qqnorm(resid(fullmodel3.1)) #qqplot
qqline(resid(fullmodel3.1)) #add the line
testDispersion(fullmodel3.1) #red line should be in the middle of the distribution
myDHARMagraph3.1<-simulateResiduals(fullmodel3.1) #testing for heteroscedasticity
plot(myDHARMagraph3.1) #plotting graph

fullmodel3.2<-lmer(log(Biomass)~Treatment+(1|Block)+(1|Population),data=mydata)
summary(fullmodel3.2)
anova(fullmodel3.2)
qqnorm(resid(fullmodel3.2)) #qqplot
qqline(resid(fullmodel3.2)) #add the line
testDispersion(fullmodel3.2) #red line should be in the middle of the distribution
myDHARMagraph3.2<-simulateResiduals(fullmodel3.2) #testing for heteroscedasticity
plot(myDHARMagraph3.2) #plotting graph

#?emmeans, emmeans(model, pairwise ~ treatment)
emmeans(fullmodel3,pairwise~Treatment)
emmeans(fullmodel3.1,pairwise~Treatment)
emmeans(fullmodel3.2,pairwise~Treatment)
#These models result in similar Tukey outcomes

####Plotting Successful Model###
ggplot(data=mydata,aes(x=Treatment,y=log(Biomass)))+geom_boxplot() #plot data from log(data)