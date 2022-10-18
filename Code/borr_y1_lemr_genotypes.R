#### Mixed Effects Model with Random Effects ####
#Dittrichia Project
#October 2022
#Miranda Melen

####Install and Load Libraries####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

#Install Packages for Analysis
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("emmeans")
#install.packages("AICcmodavg")

#Load Libraries for Analysis
library(lme4)
library(lmerTest)
library(DHARMa)
library(dplyr)
library(emmeans)
library(AICcmodavg)

####Load Data####
mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors = T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
#str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)

#?lmer #ReadMe for Fit Linear Mixed-Effects Models
#lmer(formula, data = NULL, REML = TRUE, control = lmerControl(), start = NULL, verbose = 0L, subset, weights, na.action, offset, contrasts = NULL, devFunOnly = FALSE)

#10 blocks, 5 treatments, 16 populations (CHE-O), 8 sites (random: population pairs; CHE), 2 habitat (fixed effect: roadside and off-road; R or O)

mydata$Site<-as.character(mydata$Site)

####Looking at the distribution####
#Original data
hist(mydata$Biomass, col='steelblue', main='Original') #Original data
shapiro.test(mydata$Biomass)
#Shapiro-Wilk normality test
#data:  mydata$Biomass
#W = 0.50901, p-value < 2.2e-16

#Log transform data (https://www.statology.org/transform-data-in-r/)
mydata.Log<-log10(mydata$Biomass)
hist(mydata.Log, col='coral2', main='Log Transformed') #Log transformed data
shapiro.test(mydata.Log)
#Shapiro-Wilk normality test
#data:  mydata.Log
#W = 0.95322, p-value = 1.428e-10

#Log transformed data has a better distribution than the original data

####Model 1####
fullmodel1<-lmer(log(Biomass)~ #Response variable: survival data
                         Habitat * Treatment #Fixed effects and their interactions.
                       +(1 | Site) #Random effect with random intercept only
                       +(1 | Block), #Data was blocked
                       data = mydata) #Dataframe
#isSingular(fullmodel1, tol = 1e-4) #=True
#boundary (singular) fit: see help('isSingular')
summary(fullmodel1)
#Variance explained by Site = 0.000

####Model 2####
#Modeling Site as a fixed  effect
fullmodel2<-lmer(log(Biomass)~ #Response variable: survival data
                         Habitat * Treatment + Site #Fixed effects and their interactions.
                       +(1 | Block), #Data was blocked
                       data = mydata) #Dataframe

summary(fullmodel2)
#Site as a fixed effect is not significant

####Model 3####
fullmodel3<-lmer(log(Biomass)~ #Response variable: survival data
                   Habitat * Treatment #Fixed effects and their interactions.
                 +(1 | Block), #Data was blocked, Random effect with random intercept only: Site is removed because it explains very little of the variance in the model
                 data = mydata) #Dataframe
summary(fullmodel3)
Anova(fullmodel3)

qqnorm(resid(fullmodel3)) #qqplot
qqline(resid(fullmodel3)) #add the line
testDispersion(fullmodel3) #red line should be in the middle of the distribution
myDHARMagraph3 <- simulateResiduals(fullmodel3) #making a graph using DHARMa package, also testing for heteroscedasticity ->https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#heteroscedasticity
plot(myDHARMagraph3) #plotting graph

###Compare AICs####
#Now I need to compare the AIC scores for models 2 to 3 to tell me which is the better model (but it will not say which fits my data better, that is why I did all the DHARMa stuff)

#Using aictab to make the comparison of models and table 
models <- list(fullmodel2, fullmodel3)
mod.names <- c('Site.Fixed', 'No.Site')
aictab(cand.set = models, modnames = mod.names)

#Model selection based on AICc:
#            K     AICc      Delta_AICc AICcWt Cum.Wt  Res.LL
#No.Site    12    1391.09       0.00      1      1     -683.18
#Site.Fixed 19    1412.14      21.05      0      1     -686.16

####Post-Hoc Test####
#Nicky says: you should remove non-significant interaction terms. This is what we discussed last time with Ingrid - it is difficult to judge the main effects (Habitat and Site) when you also have the interaction term in there, so when it is not significant, just fit a new model without it. This may also be a way out of your convergence errors by the way. You could try putting both random effects back in but removing Habitat: so ~ Treatment + (1|Site) + (1|Block)


#?emmeans, emmeans(model, pairwise ~ treatment)
emmeans(fullmodel3, pairwise ~ Treatment)

####Plotting Successful Model###
#ggplot(data=mydata,aes(x=Treatment,y=Biomass))+geom_boxplot() #plot data from original data

ggplot(data=mydata,aes(x=Treatment,y=log(Biomass)))+geom_boxplot() #plot data from log(data)