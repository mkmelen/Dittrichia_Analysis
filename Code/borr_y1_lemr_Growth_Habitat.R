#### Linear Mixed-Effects Models - Growth and Habitat####
#Dittrichia Project
#October 2022
#Miranda Melen

#This code uses Growth data with Habitat (roadside and off-road) and Treatment in a lmer model. Anova and Tukey tests are used on the successful ModelX with the creation of a box plot as a finished product.

#Note: 10 blocks, 5 treatments, 16 populations (CHE-O), 8 sites (random: population pairs; CHE), 2 habitat (fixed effect: roadside and off-road; R or O)

#Also Note: Growth data is a measure of growth of longest leaf. Each plant was measured upon transplanting into the field in March and then again in June 2021. This plant has a juvenile stage of a basal rosette, and then it bolts and produced smaller cauline leaves. The goal was to capture early growth data for this plant before bolting occurs so that the measurements capture the basal rosette stage, however in some cases the plants bolted earlier than expected and the resulting measurement was smaller than previous. In these cases we determined that the data should be removed from the dataset as the negative number (or changing it to a zero) does not reflect the biological importance of the measurement.

####Install Libraries ####
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("DHARMa")
#install.packages("dplyr")
#install.packages("emmeans")
#install.packages("AICcmodavg")

####Load Libraries####
library(lme4)
library(lmerTest)
library(DHARMa)
library(dplyr)
library(emmeans)
library(AICcmodavg)

####Load Data####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors=T)%>%filter(Growth>0) #Here I am only looking at the Growth data that is greater than 0 (see Also Note above)
#str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)

####Histograms####
#Original data
hist(mydata$Growth,col='steelblue',main='Original') #Original data is skewed, let's test for normality and consider log transforming the data
shapiro.test(mydata$Growth)
#Shapiro-Wilk normality test
#data:  mydata$Biomass
#W = 0.50901, p-value < 2.2e-16

#Log transform data (https://www.statology.org/transform-data-in-r/)
mydata.Log<-log10(mydata$Growth)
hist(mydata.Log,col='coral2',main='Log Transformed') #Log transformed data, this looks better than the orginal distribution
shapiro.test(mydata.Log)
#Shapiro-Wilk normality test
#data:  mydata.Log
#W = 0.95322, p-value = 1.428e-10
#Data does not improve with log transformation... try a square root

#Square root transform data
mydata.Sqrt<-sqrt(mydata$Growth)
hist(mydata.Sqrt,col='magenta',main='Square Root Transformed') #Square root transformed data
shapiro.test(mydata.Sqrt)
#Shapiro-Wilk normality test
#data:  mydata.Sqrt
#W = 0.98599, p-value = 8.66e-05
#Data distribution is improved!

####Model 1####
fullmodel1<-lmer(sqrt(Growth)~Habitat*Treatment+(1|Site)+(1|Block),data=mydata)
isSingular(fullmodel1,tol=1e-4) #=True
#boundary (singular) fit: see help('isSingular')
summary(fullmodel1) #Variance explained by Site = 0.000
Anova(fullmodel1)
#Site as a random effect does not explain any of the variance in the model, therefore let's try Site as a fixed effect to see if it adds to the model.

####Model 2####
#Modeling Site as a fixed  effect
fullmodel2<-lmer(log(Growth)~Habitat*Treatment+Site+(1|Block),data=mydata)
summary(fullmodel2) #As a fixed effect, one of the Sites (Lex) is significant.
Anova(fullmodel2)
#Site as a fixed effect accounts for 3% of the variance

#Now we'll look at the QQ plots and the residuals using DHARMa
qqnorm(resid(fullmodel2)) #qqplot
qqline(resid(fullmodel2)) #add the line
testDispersion(fullmodel2) #red line should be in the middle of the distribution
myDHARMagraph2<-simulateResiduals(fullmodel2) #making a graph using DHARMa package, also testing for heteroscedasticity, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#heteroscedasticity
plot(myDHARMagraph2) #plotting graph. At this point, you don't want any text or lines to be red.
#The QQ plots are not looking good for this model