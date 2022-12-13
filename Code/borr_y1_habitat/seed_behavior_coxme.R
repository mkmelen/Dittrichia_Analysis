#### Survival Analysis with Random Effects ####
#Dittrichia Project
#December 2022
#Miranda Melen and Nicky Lustenhouwer

#Here we will use 'coxme' which allows you to conduct mixed effects Cox proportional hazards models. Information here: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf. We conducted two germination experiments using Dittrichia graveolens seeds on filter paper (experiment 1: cox_germ_filter_paper8-2-21.csv) and seeds on two types of soil: construction fill and forest soil (experiment 2: cox_germ_soil8-2-21.csv).

#?Surv #Surv creates a survival object to combine the days column (NumDaysAlive) and the censor column (Censor) to be used as a response variable in a model formula
#The model follows the same syntax as linear models (lm, lmer, etc)
#Fixed effects: "var1 * var2" will give you the interaction term and the individual variables: so "var1 + var2 + var1:var2"
#To add random effects, type "+ (1|random effect)"

####Install Libraries####
#install.packages("coxme")
#install.packages("survival")
#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("car")
#install.packages("multcomp")

####Load Libraries####
library(coxme)
library(survival)
library(ggplot2)
library(ggfortify)
library(car)
library(multcomp)

####Load Data####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

mydata<-read.csv("data/seed_behavior/cox_germ_soil8-2-21.csv",stringsAsFactors=T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
mydata$DishNum<-as.factor(mydata$DishNum)
str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)

#This dataframe has one row per seed (1600 lines) with a calculation for how many days it took for the seed to germinate (DaysToGerm). The censor for this dataframe is CensorR with a 1 denoting a seed germinated and a 0 denoting when a seed didn't germinate by the last census date (Census = 8/2/21).

####Histograms####
#All data
hist(mydata$DaysToGerm,col='steelblue',main='Original') 

#By soil type
ggplot(mydata,aes(x=DaysToGerm))+geom_histogram()+facet_wrap(vars(Treatment)) #Here we see that Dittrichia germination responds similarly to both soil types (Treatment).

####Cox Model: Construction Fill and Forest Soils####
#Start by making a simple model with no random effects. This will be compared to the full model with random effects.
simplemodel1<-coxph(Surv(DaysToGerm,Censor)~Habitat*Treatment,data=mydata)
summary(simplemodel1)
print(simplemodel1)
predict(simplemodel1)
hist(predict(simplemodel1))

#Now make a full model using random effects
fullmodel1<-coxme(Surv(DaysToGerm,Censor)~Habitat*Treatment+(1|Site/Population/DishNum),data=mydata)
summary(fullmodel1)
print(fullmodel1)
predict(fullmodel1)
hist(predict(fullmodel1))

#Now we can compare the models to see which model is best
anova(simplemodel1,fullmodel1) #There is a significance between models
#See example: https://www.rdocumentation.org/packages/coxme/versions/2.2-16/topics/coxme
AIC(simplemodel1,fullmodel1) #But comparing the AIC scores is easiest. Keep the lower AIC score because that is considered the better model
#              df      AIC
#simplemodel1  3.00000 16228.77
#fullmodel1   22.59853 16136.01
#The full model (fullmodel1) is best

#Now, let's make a model with no interaction term
fullmodel2<-coxme(Surv(DaysToGerm,Censor)~Habitat+Treatment+(1|Site/Population/DishNum),data=mydata) 
summary(fullmodel2)

#Let's compare the the first two models to test for the significance of the term that is removed (using LR)
anova(fullmodel1,fullmodel2) #Not significant
AIC(fullmodel1,fullmodel2) #The two models are within 2 points of each other, so I can use the simpler model, which means the interaction term is dropped and we keep fullmodel2.

#Here's the model we keep:
summary(fullmodel2)
#Fixed coefficients
#                  coef          exp(coef)   se(coef)     z      p
#HabitatRoadside   0.040964779   1.041815    0.15442210   0.27   0.79
#TreatmentForest   0.002252879   1.002255    0.05599009   0.04   0.97

####Risk Assessment####
#Let's pull out some coefficients, which tell us about the risk of death. 1 = no effect, <1 = decreased risk of death, >1 = increased risk of death.

exp(coef(fullmodel2))
# HabitatRoadside  TreatmentForest 
#1.041815          1.002255 
#Note that these are also found in the summary(model). When we run this, both are just above 1, which means that roadside has an ever so so slight increase in germination compared to off-road, and forest has an even smaller slight increase in germination compared to construction.

#Here we pull the std dev of the random effects which works like the fixed effect coefficients to calculate risk.
exp(ranef(fullmodel2)$Site) # Pretty good for most, CHE is lower
#BAY       CHE       GUA       LEX       OAP       PAR       PEN       SSJ 
#0.9993086 1.0030391 0.9969110 0.9997749 0.9987377 0.9998305 1.0027064 0.9997059 

####Plots####
#Plot by habitat
model_graph1<-survfit(Surv(DaysToGerm,Censor)~Habitat,data=mydata)
autoplot(model_graph1)+labs(x="\n Time to Germination (Days)",y="Germination Probabilities\n",title="Germination Times Of Seeds from \n Roadside and Adjacent Vegetation\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))

#Plot by habitat and treatment
model_graph2<-survfit(Surv(DaysToGerm,Censor)~Habitat+Treatment,data=mydata)
autoplot(model_graph2)+labs(x="\n Time to Germination (Days)",y="Germination Probabilities\n",title="Germination Times Of Seeds from \n Roadside and Adjacent Vegetation\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))

#Plot by site and treatment
model_graph3<-survfit(Surv(DaysToGerm,Censor)~Site+Treatment,data=mydata)
autoplot(model_graph3)+labs(x="\n Time to Germination (Days)",y="Germination Probabilities\n",title="Germination Times Of Seeds from \n Roadside and Adjacent Vegetation\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))