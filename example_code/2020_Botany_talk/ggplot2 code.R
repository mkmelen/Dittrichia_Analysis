## This code makes point graphs using ggplot2 and was used in the production of 4 graphs for a talk on Dittrichia graveolens for the Botany 2020 conference.

#### ===== libraries =====
library(ggplot2)
library(sciplot)

# set working directory
setwd("~/Documents/Education/R Directory/DIGR/2020_Botany_talk")

# load data
digr <- read.csv("F1_harvest_results_withbioclim.csv", header=T, stringsAsFactors = T) # reads the .csv into the file "digr", noting there are headers
seedtraits <- read.csv("seedtraitspilot.csv", header=T, stringsAsFactors=T)

# check your data
class(digr) # lets you ask what the class of "digr" is
str(digr) # structure of the dataframe
summary(digr) # summary table of dataframe
head(digr) # shows first 6 lines of dataframe
tail(digr) # shows last 6 lines of dataframe

class(seedtraits) # lets you ask what the class of "seedtraits" is
str(seedtraits) # structure of the dataframe
summary(seedtraits) # summary table of dataframe
head(seedtraits) # shows first 6 lines of dataframe
tail(seedtraits) # shows last 6 lines of dataframe

str(seedtraits)
names(seedtraits)[1] <- "id"
names(seedtraits)[2] <- "pop"

# some basic data operations
digr$fam <- as.factor(digr$fam) # set family as factor
str(digr)
levels(digr$fam)
levels(digr$pop)

digr[1:10,"id"] # rows, columns
digr$id
x <- c(1,2,3,4,5,6)
x[3]


#### ===== estimate number of seeds per plant =====

# calculate average 
seeds_per_head <- aggregate(nr_seeds ~ region, data=seedtraits, mean, na.rm=T)
seeds_per_head
names(seeds_per_head)[2] <-"nr_seeds_avg"

# multiply by nr of seed heads
digr <- merge(digr, seeds_per_head, by="region", all.x=T) # add region averages to digr dataframe
digr$nrseeds_est <- digr$seedheads_total * digr$nr_seeds_avg
names(digr)

#### ===== Graphs =====

#make a graph with points

#quick way to visualize data in a barplot
bargraph.CI(region, nrseeds_est, data=digr)

# ways to summarize data in R

# for making summary table: FUN=mean is average, FUN=se is standard error, FUN=sd is standard deviation

# F1 data, make average and standard error
names(digr)
digr.summary <- aggregate(cbind(stem_length, nrseeds_est) ~ region, data=digr, FUN=mean, na.rm=T)  # summarize by region
digr.summary$stem_length_se <- tapply(digr$stem_length, digr$region, se, na.rm=T)
digr.summary$nrseeds_est_se <- tapply(digr$nrseeds_est, digr$region, se, na.rm=T)
str(digr.summary)

# pilot data, make average and standard error
names(seedtraits)
seedtraits.summary <- aggregate(cbind(seed_weight.mg._per_seed) ~ region, data=seedtraits, FUN=mean, na.rm=T)  # summarize by region
seedtraits.summary$seed_weight.mg._per_seed_se <- tapply(seedtraits$seed_weight.mg._per_seed, seedtraits$region, se, na.rm=T)
str(seedtraits.summary)

# F1 data, change codes to names
levels(digr.summary$region)
digr.summary$region <- factor(digr.summary$region, levels=c("FS","FC","NL","CC","CE"),
                              labels=c("Southern France", "Central France", "Netherlands", "California Core", "California Edge"))

# pilot data, change codes to names
levels(seedtraits.summary$region)
seedtraits.summary$region <- factor(seedtraits.summary$region, levels=c("FS","FC","NL","CC","CE"),
                              labels=c("Southern France", "Central France", "Netherlands", "California Core", "California Edge"))


# since there is no BIO6 for FBK, we need to remove and make a subset
digr.nofbk <- subset(digr, pop != "CC-FBK") # removes the rows where pop=CC-FBK
digr.nofbk$pop <- droplevels(digr.nofbk$pop) # drops unused levels (i.e. facebook )

digr.summary.pop <- aggregate(cbind(stem_length, nrseeds_est, bio6, latitude, longitude, region) ~ pop, data=digr.nofbk, FUN=mean, na.rm=T) # summarize by population
digr.summary.pop$stem_length_se <- tapply(digr.nofbk$stem_length, digr.nofbk$pop, se, na.rm=T)
digr.summary.pop$nrseeds_est_se <- tapply(digr.nofbk$nrseeds_est, digr.nofbk$pop, se, na.rm=T)
str(digr.summary.pop)
digr.summary.pop$region <- factor(digr.summary.pop$region, levels=c(4,3,5,1,2), labels=c("Southern France", "Central France", "Netherlands", "California Core", "California Edge"))
digr.summary.pop

# save colors
pal1 <- c("darkorange","gold1","deepskyblue1","red4","red2")
pal.nicky <- c("yellow2","cornflowerblue","darkorange","green4","green3")

# for a graph with points = geom_point, box plot = geom_box

# Estimated number of seeds
ggplot(digr.summary, aes(x=region, y=nrseeds_est,col=region))+
  geom_errorbar(aes(ymin=nrseeds_est-nrseeds_est_se, ymax=nrseeds_est+nrseeds_est_se), width=.1, size=1)+
  geom_point(size=4)+
  scale_colour_manual(values=pal1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text = element_text(size=14), axis.title = element_text(size=16)) +
  labs(x=" ",y="Estimated Number of Seeds") 

# Plant height
ggplot(digr.summary, aes(x=region, y=stem_length,col=region))+
  geom_errorbar(aes(ymin=stem_length-stem_length_se, ymax=stem_length+stem_length_se), width=.1, size=1)+
  geom_point(size=4)+
  scale_colour_manual(values=pal1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
        axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x=" ",y="Plant Height") 

# Seed weight
ggplot(seedtraits.summary, aes(x=region, y=seed_weight.mg._per_seed,col=region))+
  geom_errorbar(aes(ymin=seed_weight.mg._per_seed-seed_weight.mg._per_seed_se, ymax=seed_weight.mg._per_seed+seed_weight.mg._per_seed_se), width=.1, size=1)+
  geom_point(size=4)+
  scale_colour_manual(values=pal1)+
  scale_y_continuous(limits=c(.18,.32))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
        axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x=" ",y="Seed Weight (mg)") 

# for formatting a graph
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
      axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x="Origin",y="Estimated Number of Seeds", title="your plot title") 
# nice theme: theme_bw()    


# plot stem_length ~ bio6: all regions
ggplot(digr.summary.pop, aes(x=bio6, y=stem_length, col=region)) +
  stat_smooth(method="lm", formula = y ~ x + I(x^2), se=T, inherit.aes=F, aes(x=bio6, y=stem_length), color="black") +
  geom_errorbar(aes(ymin=stem_length-stem_length_se, ymax=stem_length+stem_length_se), width=.1, size=1) +
  geom_point(size=4) +
  scale_colour_manual(values=pal1) +
  scale_y_continuous(limits=c(40,90))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
        axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x="Minimum Temperature of the Coldest Month",y="Plant Height (cm)") 

# plot stem_length ~ bio6: only Europe
Europe <- c("Southern France","Central France", "Netherlands")
ggplot(digr.summary.pop, aes(x=bio6, y=stem_length, col=region)) +
  stat_smooth(method="lm", formula = y ~ x + I(x^2), se=T, inherit.aes=F, aes(x=bio6, y=stem_length), color="black") +
  geom_errorbar(aes(ymin=stem_length-stem_length_se, ymax=stem_length+stem_length_se), width=.1, size=1, data=subset(digr.summary.pop, region %in% Europe)) +
  geom_point(size=4, data=subset(digr.summary.pop, region %in% Europe)) +
  scale_colour_manual(values=pal1) +
  scale_y_continuous(limits=c(40,90))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
        axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x="Minimum Temperature of the Coldest Month",y="Plant Height (cm)") 

###
# plot stem_length ~ latitude: all regions
ggplot(digr.summary.pop, aes(x=latitude, y=stem_length, col=region)) +
  stat_smooth(method="lm", formula = y ~ x, se=T, inherit.aes=F, aes(x=latitude, y=stem_length), color="black") +
  geom_errorbar(aes(ymin=stem_length-stem_length_se, ymax=stem_length+stem_length_se), width=.1, size=1) +
  geom_point(size=4) +
  scale_colour_manual(values=pal1) +
  scale_y_continuous(limits=c(40,90))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), 
        axis.text = element_text(size=10), axis.title = element_text(size=13)) +
  labs(x="Latitude",y="Plant Height (cm)") 

# example of linear model statistics  
plot(stem_length ~ bio6, digr.summary.pop, pch=16)  

model.linear <- lm(stem_length ~ bio6, digr.summary.pop) # linear model
summary(model.linear)
#plot(model.linear) # check model assumptions
abline(model.linear) # add to plot

model.quadratic <- lm(stem_length ~ bio6 + I(bio6^2), digr.summary.pop)
summary(model.quadratic)
AIC(model.linear, model.quadratic)



