## Coxme example with Streetfighter data ##

# Nicky Lustenhouwer & Erin Aiello

library(coxme)
library(car)
library(survival)
library(multcomp)

#library(tidyr)
#library(ggfortify)
#library(RColorBrewer)
#library(survMisc)
#library(ggplot2)

setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis/example_code/coxme")

## Get data
survdrt <- read.csv("sf_cox_drought.csv",stringsAsFactors = T)
head(survdrt) #  here you can see the first 6 rows of your dataframe
str(survdrt) # here you need to make sure that each column has the right class (factor, integer, numeric, etc.)

# this data frame has one row per plant. Erin was recording the number of days it takes for the plant to die;
# you will have one row per seed, and you are writing down how many days it took for the seed to germinate.

# days_surv shows how many days the plant survived; in your case this will be the day of germination, or the last day your surveyed the ungerminated seed
# censor indicates what happened on this day; either the plant died (1) or the plant was still alive at the end of the experiment (0)
# for you, this will be the seed germinated (1) on this day, or the seed still didn't germinate on this day (0)


######################## running the cox proportional hazards survival test with random factors #####################

?Surv         #Surv creates a survival object
# here you combine the days column (days_surv) and the censor column (with your 1s and 0s)

# the model follows the same syntax as linear models (lm, lmer, etc)
# fixed effects: "var1 * var2" will give you the interaction term and the individual variables: so "var1 + var2 + var1:var2"
# to add random effects, type "+ (1|random effect)"

fullmodel <- coxme(Surv(days_surv,censor) ~  # response variable: survival data
                     inv*soil + drought_trt*soil + drought_trt*inv + drought_trt*soil*inv + # fixed effects and their interactions
                     (1|df_type) + (1|block) + (1|siteID), # random effects
                     survdrt) # dataframe

# I think your model will look something like this:
#fullmodel <- coxme(Surv(day, censor) ~ PopType * DishType + (1|PopName) + (1|Rep), yourdata)
# or maybe like this. Usually R will know that Rep is nested, it may go wrong if you try to specify it like this:
#fullmodel <- coxme(Surv(day, censor) ~ PopType * DishType + (1|PopName) + (1|PopName/PopType/Rep), yourdata)

# note the warning message; this means we probably have too much stuff in our model (and not enough data points) to fit it properly
# for the sake of this exercise we will continue though:

?Anova
Anova(fullmodel,test.statistic="LR") # type II likelihood ratio test to test your fixed effects
# It is confusing that you write Anova here (the capital A is important) because this is not really an anova; 
# R uses this function for lots of different tests of the main variables in your model.

# Erin's results show that none of the interaction terms are significant, but the main effects (inv, soil, drought_trt) are.
# We therefore make a reduced model where we remove the interactions, but we keep the random effects:

reduced.model <- coxme(Surv(days_surv,censor) ~      # response variable: survival data
                         inv + soil + drought_trt +  # fixed effects
                         (1|df_type) + (1|block) + (1|siteID), # random effects
                         survdrt) # dataframe

Anova(reduced.model,test.statistic="LR") # inv, soil, and drought_trt all have a significant effect on survival
summary(reduced.model) # here you can see a full summary of the model

# If your fixed effect has more than 2 levels, you can also compare them individually with a post-hoc test like this.
# I don't think you'll need this because your fixed effect (road or off-road) has only two levels. Same for Erin:
summary(glht(reduced.model, mcp(inv="Tukey"))) # this will give you the same P-value as the Anova above for "inv"


# We now want to know for each variable whether it increases or decreases the risk of death (the event that is censored; in your case this will be the probability of germination)
# Let's see what the levels of Erin's variables are:
levels(survdrt$inv) # we have "Br" = broom and "Un" = univaded soil
levels(survdrt$soil) # "L" = live soil and "S" = sterile soil
levels(survdrt$drought_trt) # there is a "drought" and a "wet" treatment
# you will have two fixed effects: PopType (with "road" and "off-road" as levels), and DishType ("forest" and "construction")

exp(coef(reduced.model)) # we back-transform the coefficients in our model (they were on a log scale) so we can interpret them:
# R shows one of the levels and used the other as a reference: invUn (uninvaded soil), soilS (sterile soil), and drought_trtwet (wet treatment)
# the coefficients for all of these levels are <1, which means that they give a reduced risk of death
# --> plants have a lower probablity of dying in univaded, sterile soil, in the wet treatment (0.005 is MUCH lower than 1)

# numbers will give you the risk of death, compared to 1.
# so: plants in the wet treatment have a
1 - exp(coef(reduced.model))[3] # 99.5% reduced risk of death.
# being in sterile soil is making you 40% less likely to die
# being uninvaded makes plants ~ 85% less likely to die.
# because these are probabilities (which are multiplicative), it looks like there is a drought * invasion interaction, but there is none.

# for your experiment, the coefficients  will show you whether seeds will have a higher or lower probability of germinating,
# depending on whether they are roadside/off-road, and depending on the soil type

## Now let's have a look at the random effects.
# in Erin's study, we wanted to see if results differed between plant genotypes, blocks, and sites:
exp(ranef(reduced.model)$df_type) # street fighter 2 has a reduced risk of dying (below 1)
exp(ranef(reduced.model)$block) # yay, small differences in risk between blocks; some have slightly lower (<1) or higher (>1) chances of dying
exp(ranef(reduced.model)$siteID) # results are very similar between sites

# for your experiment, this will show the difference between Rep dishes, for example, and the difference between locations (pairs)


### Code to make some graphs ###
# you don't need this because you already made nice graphs in JMP! But just so you can have a look at Erin's data:
# this code doesn't work with random effects so we fit a new model that doesn't have them:
noranef <-  coxph(Surv(days_surv,censor) ~
                    inv +
                    soil +
                    drought_trt,
                    survdrt)

plot(survfit(noranef, conf.int=F))  # overall plot
combinations <- expand.grid(inv=levels(survdrt$inv), soil=levels(survdrt$soil), drought_trt=levels(survdrt$drought_trt))
combinations
plot(survfit(noranef, newdata = combinations), conf.int=F, 
     col = rep(c("chartreuse3","chartreuse3","brown","brown"),2),  # green=live, red=sterile
     lty = c(5,4,5,4,1,3,1,3))

plot(survfit(noranef, newdata = combinations), conf.int=F, 
     col = c(rep("brown",4), rep("blue",4)),  # blue=wed, red=dry
     lwd = c(2,1,2,1,2,1,2,1), # invasion lines are thicker
     lty = c(3,3,1,1,3,3,1,1))  # live=dotted, sterile=continuous

plot(survfit(noranef, newdata = combinations), conf.int=F, 
     col = c(rep("brown",4), rep("blue",4)),  # blue=wed, red=dry
     lwd = c(2,1,2,1,2,1,2,1), # invasion lines are thicker
     lty = c(3,3,1,1,3,3,1,1),  # live=dotted, sterile=continuous
     ylim=c(.9,1))  


# make up data
x <- seq(1,20,1)
y <- rnorm(20)
plot(y~x)
mod <- lm(y~x)
summary(mod)
Anova(mod, test.statistic="LR")
