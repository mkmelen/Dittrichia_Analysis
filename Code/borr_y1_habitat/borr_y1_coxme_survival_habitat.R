#### Survival Analysis with Random Effects ####
#Dittrichia Project
#October 2022
#Miranda Melen and Nicky Lustenhouwer

#Here we will use 'coxme' which allows you to conduct mixed effects Cox proportional hazards models. Information here: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf.

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

mydata<-read.csv("data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors=T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)
mydata$Site<-as.character(mydata$Site)
mydata$Treatment<-factor(mydata$Treatment,levels=c("Control","Biomass Removal","Hemizonia","Raking","Raking + Clipping")) #Changing the contrast order so that everything is compared to Control

#This dataframe has one row per plant with a calculation for how many days it took for the plant to reproduce (NumDaysAlive). The censor for this dataframe is Censor with a 1 denoting a plant lived and a 0 denoting plant died by the last census date. Erin says dying is 1 and 0 alive, so I need to double check this.

####Histograms####
#Assumptions for cox models: https://www.theanalysisfactor.com/assumptions-cox-regression/

#All data
hist(mydata$NumDaysAlive,col='steelblue',main='Original') 

#By Treatment
ggplot(mydata,aes(x=NumDaysAlive))+geom_histogram()+facet_wrap(vars(Treatment)) #Here we see that 4 of the treatments have the same bi-modal distribution and Biomass Removal is right skewed.

####Cox Models####
#Start by making a simple model with no random effects. This will be compared to the full model with random effects.
simplemodel1<-coxph(Surv(NumDaysAlive,Censor)~Habitat*Treatment,data=mydata)
summary(simplemodel1)
print(simplemodel1)
predict(simplemodel1)
hist(predict(simplemodel1))

#Now make a full model using random effects
fullmodel1<-coxme(Surv(NumDaysAlive,Censor)~Habitat*Treatment+(1|Site)+(1|Block),data=mydata)
summary(fullmodel1)
print(fullmodel1)
predict(fullmodel1)
hist(predict(fullmodel1))

#Now we can compare the models to see which model is best
anova(simplemodel1,fullmodel1) 
#See example: https://www.rdocumentation.org/packages/coxme/versions/2.2-16/topics/coxme
AIC(simplemodel1,fullmodel1) #But comparing the AIC scores is easiest. Keep the lower AIC score because that is considered the better model
#              df      AIC
#simplemodel1  9.00000 9070.615
#fullmodel1   20.33324 9043.518
#The full model (fullmodel1) is best

#Ideally we would like to pull p-values, however we need to use likelihood ratio and it doesn't work properly right now... perhaps broken?
#Anova(fullmodel1, test="LR") #Doesn't work :-(
#Anova(fullmodel1, test="Wald") #Does work, but we want LR (likelihood ratio)

#Now, let's make a model with no interaction term
fullmodel2<-coxme(Surv(NumDaysAlive,Censor)~Habitat+Treatment+(1|Site)+(1|Block),data=mydata) 
summary(fullmodel2)

#Let's compare the the first two models to test for the significance of the term that is removed (using LR)
anova(fullmodel1,fullmodel2) #Not significant
AIC(fullmodel1,fullmodel2) #Interaction term is not significant! So we can drop the interaction term and keep fullmodel2
#           df      AIC
#fullmodel1 20.33324 9043.518
#fullmodel2 16.34036 9037.097

#So, now let's add in population nested under site as a random effect
fullmodel3<-coxme(Surv(NumDaysAlive,Censor)~Habitat+Treatment+(1|Site/Population)+(1|Block),data=mydata) 
summary(fullmodel3)

#Now we can compare the models to see which model is best
anova(fullmodel2,fullmodel3) #Almost significant
AIC(fullmodel2,fullmodel3) #Looks like fullmodel3 is the better model
#           df      AIC
#fullmodel2 16.34036 9037.097
#fullmodel3 20.21425 9030.281

####Risk Assessment####
#Let's pull out some coefficients, which tell us about the risk of death. 1 = no effect, <1 = decreased risk of death, >1 = increased risk of death. NOTE which model is being used.
exp(coef(fullmodel2))
# HabitatRoadside TreatmentBiomass Removal TreatmentHemizonia TreatmentRaking TreatmentRaking + Clipping 
#1.0722847        0.6363994                1.1877251          1.2564276       1.3111230 

exp(coef(fullmodel3))
#HabitatRoadside TreatmentBiomass Removal TreatmentHemizonia TreatmentRaking TreatmentRaking + Clipping
#1.0714836       0.6367177                1.1904400          1.2563844       1.3259674 

#Here we pull the std dev of the random effects which works like the fixed effect coefficients to calculate risk. NOTE which model is being used.
exp(ranef(fullmodel2)$Site) # Pretty good for most, CHE is lower
#BAY       CHE       GUA       LEX       OAP       PAR       PEN       SSJ 
#1.1171091 0.8140497 1.0873108 1.0204675 0.8699171 1.1470097 1.0133152 0.9801920 

exp(ranef(fullmodel2)$Block) # Pretty good for most, G and J are lower
#  A         B         C         D         E         F         G         H         I         J 
#1.3171942 1.1706509 0.9901219 0.8838112 1.1412207 1.0962480 0.8086329 0.9393610 0.9671968 0.8063013 

exp(ranef(fullmodel3)$"Site/Population")
#BAY/BAY-O BAY/BAY-R CHE/CHE-O CHE/CHE-R GUA/GUA-O GUA/GUA-R LEX/LEX-O LEX/LEX-R OAP/OAP-O OAP/OAP-R PAR/PAR-O 
#1.0615692 1.0733377 0.9154462 0.8414985 1.1541189 0.9631803 1.0128569 1.0059675 0.7876246 1.0854807 1.1327600 
#PAR/PAR-R PEN/PEN-O PEN/PEN-R SSJ/SSJ-O SSJ/SSJ-R 
#1.0421698 1.1077178 0.9189305 0.8907050 1.0991970

Anova(fullmodel3)
#Analysis of Deviance Table (Type II tests)
#Response: Surv(NumDaysAlive, Censor)
#Df  Chisq Pr(>Chisq)    
#Habitat    1  0.414     0.5199    
#Treatment  4 52.139   1.29e-10 ***
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Looks like roadside and offroad plants are the same, so let's combine them together in a model (aka, removing the Habitat term)
fullmodel4<-coxme(Surv(NumDaysAlive,Censor)~Treatment+(1|Site/Population)+(1|Block),data=mydata) 
summary(fullmodel4)
summary(fullmodel3)

####Plots####
#Plot by habitat
model_graph1<-survfit(Surv(NumDaysAlive,Censor)~Habitat,data=mydata)
autoplot(model_graph1)+labs(x="\n Survival Time (Days)",y="Survival Probabilities\n",title="Survival Times Of \n Roadside and Off-road Populations\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))

#Plot by treatment
model_graph2<-survfit(Surv(NumDaysAlive,Censor)~Treatment,data=mydata)
autoplot(model_graph2)+labs(x="\n Survival Time (Days)",y="Survival Probabilities\n",title="Survival Times Of \n Roadside and Off-road Populations\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))

####Post hoc Tukey####
summary(glht(fullmodel4, mcp(Treatment="Tukey")))
?glht #mpc = multiple comparison
