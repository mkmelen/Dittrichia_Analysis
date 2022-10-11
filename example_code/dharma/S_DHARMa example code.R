# THIS SCRIPT USES DHARMA TO ASSESS GLMM MODELS USING COVER DATA
# Applied here on: data from surveys at Hester in 2021
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock" and R Studio Version 1.0.153 – © 2009-2017
# Last updated 09/18/2021 by Karen Tanner 



##### SETUP ####
#set working directory
setwd("~/Documents/Education/UC Santa Cruz/Dittrichia/Analysis/Karen Tanner Data - GLMM example")
# store my target working directory as "dir"
dir = '/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Analysis/Karen Tanner Data - GLMM example' 
#dir = '/Users/karentanner/R Playing/Data and Scripts/Skills/DHARMa' 
# define dir as the working directory
setwd(dir)
# list the files present in the working directory
list.files()

# load libraries
library(dplyr)
library(lme4)
library(car)
library(DHARMa)

#### LOAD FILES #####
data <-as.data.frame(read.csv ("Data_native_cover_2021.csv"))
# this is data on marsh plant cover along a linear transect; each pin drop is a 1 or a 0, so we will use a binomial (Bernoulli coin flip) approach here in a GLMM
# need to do some reorganization of the data, because I only want to look at Frankenia cover in data rows with Species=F, etc 
F_data<-data %>% filter(Species=="F") %>% group_by(Block,Species,Pattern,Elevation,Division)%>% summarise(Present = (Frankenia))
str(F_data)
F_data$Block<-as.factor(F_data$Block) # make Block a factor




#### MODELS  ####
# https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
# have to set contrasts to get Type III SS done correctly
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/unbalanced.html
options("contrasts")
options(contrasts=c("contr.sum","contr.poly"))
options("contrasts")

# Bernoulli coin flip model, so we don't have to worry about overdispersion:
# https://stackoverflow.com/questions/22842017/model-checking-and-test-of-overdispersion-for-glmer/28672386
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
# https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models


# Model Frankenia data
model_F<-glmer(Present~Elevation*Pattern + (1|Block),data=F_data,subset=Species=="F",family="binomial"); Anova(model_F, type=3)

# Assess GLMM with DHARMa
sim_output_model_F <- simulateResiduals(fittedModel = model_F, plot = F) # save simulated data to work with
# QQ plot
plotQQunif(sim_output_model_F) # ok
# standardized residuals vs model predictions plot
plotResiduals(sim_output_model_F); testQuantiles(sim_output_model_F) # ok
# inspect further by looking at standardized residuals vs individual predictors...Elevation and Pattern (clustered or uniform)
plotResiduals(sim_output_model_F, F_data$Elevation)
testCategorical(sim_output_model_F, catPred = F_data$Elevation)
plotResiduals(sim_output_model_F, F_data$Pattern)
testCategorical(sim_output_model_F, catPred = F_data$Pattern) 


