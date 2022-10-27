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
#install.packages("survival")
#install.packages("ggplot2")
#install.packages("ggfortify")

####Load Libraries####
library(coxme)
library(survival)
library(ggplot2)
library(ggfortify)

####Load Data####
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis")

mydata<-read.csv("Data/blue_oak_yr1/blue_oak_competition_datasheet_2021_phenology_survey.csv",stringsAsFactors=T)
#head(mydata)#Let's look at the first 6 rows of the dataframe
str(mydata) #Check that each column has the right class (factor, integer, numeric, etc.)
mydata$Site<-as.character(mydata$Site)

#This dataframe has one row per plant with a calculation for how many days it took for the plant to reproduce (NumDaysAlive). The censor for this dataframe is Censor with a 1 denoting a plant lived and a 0 denoting plant died by the last census date. Erin says dying is 1 and 0 alive, so I need to double check this.

####Histograms####
#Assumptions for cox models: https://www.theanalysisfactor.com/assumptions-cox-regression/

#All data
hist(mydata$NumDaysAlive,col='steelblue',main='Original') 

#By Treatment
ggplot(mydata,aes(x=NumDaysAlive))+geom_histogram()+facet_wrap(vars(Treatment)) #Here we see that 4 of the treatments have the same bi-modal distribution and Biomass Removal is right skewed.

####Cox Model####
#Start by making a simple model with no random effects. This will be compared to the full model with random effects.
simplemodel1<-coxph(Surv(NumDaysAlive,Censor)~Habitat*Treatment,data=mydata)
print(simplemodel1)

fullmodel1<-coxme(Surv(NumDaysAlive,Censor)~Habitat*Treatment
                  +(1|Site)
                  +(1|Block),
                  data=mydata)
print(fullmodel1)

#Compare the models
anova(simplemodel1,fullmodel1) #See example: https://www.rdocumentation.org/packages/coxme/versions/2.2-16/topics/coxme

fullanova<-Anova(fullmodel1)
print(fullanova)

simpleanova<-Anova(simplemodel1)
print(simpleanova)


stem(exp(ranef(fullmodel1)[[1]]))

#Test code to plot by habitat
model_graph1<-survfit(Surv(NumDaysAlive,Censor)~Habitat,data=mydata)
autoplot(model_graph1)+labs(x="\n Survival Time (Days)",y="Survival Probabilities\n",title="Survival Times Of \n Roadside and Off-road Populations\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))

#Test code to plot by treatment
model_graph2<-survfit(Surv(NumDaysAlive,Censor)~Treatment,data=mydata)
autoplot(model_graph2)+labs(x="\n Survival Time (Days)",y="Survival Probabilities\n",title="Survival Times Of \n Roadside and Off-road Populations\n")+theme(plot.title=element_text(hjust=0.5),axis.title.x=element_text(face="bold",color="Black",size = 12),axis.title.y=element_text(face="bold",color="Black",size=12),legend.title=element_text(face="bold",size=10))


#ERROR WITH ANOVA # Error in update.default(formula(object), formula.) : need an object with call component
Anova((fullmodel1), test = "LR") #Use a type II likelihood ratio test to test your fixed effects
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