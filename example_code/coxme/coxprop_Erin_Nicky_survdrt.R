#Using RStudio 1.2.5019, R 3.6.1, MacOS Monterey 12.3.1
setwd("/Users/Miranda/Documents/Education/UC Santa Cruz/Dittrichia/Dittrichia_Analysis/example_code/coxme")
library(coxme)
library(car)
library(tidyr)
library(ggfortify)
library(survival)
library(RColorBrewer)
library(survminer)
library(survMisc)
library(ggplot2)
library(multcomp)

survdrt <- read.csv("SFcox_drought_sterile.csv")

######################## running the cox proportional hazards survival test with random factors #####################

#?Surv         #Surv creates a survival object
# to add random effects, type "+ (1|random effect)"

######### below: includes wet treated, so drought treatment is a factor ###########

fullmodel <- coxme(Surv
                   (days_surv,censor) ~
                     inv +
                     drought_trt +
                     drought_trt*inv +
                     (1|df_type) +
                     (1|block) +
                     (1|siteID),
                   survdrt)

anova(fullmodel)
nova <- Anova(fullmodel,test="LR") # type II likelihood ratio test to test your fixed effects
nova

reduced.model <- coxme(Surv                           #because the interaction term was insignificant
                       (days_surv,censor) ~
                         inv +
                         drought_trt +
                         (1|df_type) +
                         (1|block) +
                         (1|siteID),
                       survdrt)

nova.add <- Anova(reduced.model,test="LR")
nova.add
summary(reduced.model)

exp(coef(reduced.model))

summary(reduced.model)  # the std dev of your random effects work like the fixed effect coefficients

exp(ranef(reduced.model)$df_type) # street fighter 2 has a reduced risk of dying (all blocks had values b/w 0.9991 and 1.0006)
exp(ranef(reduced.model)$block) # yay,small differences in risk between blocks
exp(ranef(reduced.model)$siteID) # sites don't matter

library(sciplot)
bargraph.CI(drought_trt, days_surv, group=inv, data=subset(survdrt, soil=="L"), 
            ylim=c(0,100), legend=T, ylab="days surviving", main="live soil")

bargraph.CI(drought_trt, days_surv, group=inv, data=subset(survdrt, soil=="S"), 
            ylim=c(0,100), legend=T, ylab="days surviving", main="sterile soil")



noranef <-  coxph(Surv(days_surv,censor) ~
                                inv +
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

exp(coef(reduced.model))  # numbers will give you the risk of death, compared to 1.
# so: plants in the wet treatment have a
1 - exp(coef(reduced.model))[3] # 99.5% reduced risk of death.
# being in sterile soil is making you 40% less likely to die
# being uninvaded makes plants ~ 85% less likely to die.
# because these are probabilities (which are multiplicative), it looks like there is a drought * invasion interaction, but there is none.
