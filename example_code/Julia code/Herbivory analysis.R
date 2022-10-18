# Hebivory analysis: Costus allenii vs. C. villosissimus
# Spoiler - The vill always wins
# Julia Harenčár
# Last updated 220503

setwd("/Users/Julia/Library/CloudStorage/GoogleDrive-jharenca@ucsc.edu/My Drive/Costus/Herbivory")

library("tidyverse")
library("MASS") #does not work with dplyr select because also has a select function!
library("dplyr")
library("lme4")
library("car")
library("ggplot2")
library("patchwork")
library("RColorBrewer")
library("DHARMa")
theme_set(theme_bw())

###  Rolled leaf beetle presence/absence ----------------------------------------------------------------------
# RLB <- read.csv("RLB_PA.csv", header=TRUE)
# 
# alleRLBr <- RLB$X.RLBs[RLB$HostSpecies == "alle"] #subsetting the RLB counts for alle
# villRLB <- RLB$X.RLBs[RLB$HostSpecies == "vill"] #subsetting the RLB counts for vill
# alleRLB <- alleRLBr/RLB$X.RolledLeaves[RLB$HostSpecies == "alle"] #dividing the number of RLBs by the number of RLs to get a plant average. I do not do this for vill because it is all 0s.
# 
# #Chi squared of presence absence 
# present <- c(sum(alleRLB != 0), sum(villRLB != 0))
# absent <- c(sum(alleRLB == 0), sum(villRLB == 0))
# RLBChiTable <- data.frame(present,absent)
# row.names(RLBChiTable) <- c("alle", "vill")
# chisq.test(RLBChiTable, simulate.p.value=TRUE, B=1e7)
# 
# # Making a simple barplot of presence/absence colonization data
# spp <- c("C. allenii", "C. villosissimus")
# per <- c(27.8, 0)
# N <- c("n=18", "n=43")
# beetle_PA_df <- data.frame(spp, per, N)
# PA <- ggplot(data=beetle_PA_df, aes(x=spp, y=per)) +
#   geom_bar(stat="identity", fill="black") + 
#   ggtitle("beetle colonization") + 
#   ylim(0, 30.3) +
#   geom_text(label = N, vjust= -.5, size = 3) +
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   xlab("") + 
#   ylab('percent colonizatoin') + 
#   scale_x_discrete(labels=c("C. allenii" = expression(italic("C. allenii")), "C. villosissimus" = expression(italic("C. villosissimus")))) + 
#   theme(axis.text.x = element_text(angle = 15, hjust = 0.6))
# 
# ggsave("beetle_colonization_plot.png", device = "png", width = 5, height = 6, units = "cm")

#t.test(villRLB, alleRLB)#t-test to compare the number of RLB found on alle to those found on vill. Problem with 0s?!

#poisson.test(villRLB[1:18], alleRLB) #I thought this made sense because they are technically count data, but it isn't working... first the sampl sizes have to be equal, but now it throws an error I don't understand: "the case k > 2 is unimplemented"

#villRLB1 <- villRLB + 1
#alleRLB1 <- alleRLB + 1 
#t.test(villRLB1, alleRLB1)#t-test after adding one to everything to correct for problems with the 0s... IS THIS THE BEST WAY? Must not be because it gives the same exact values... As count data, it has a poisson, not normal distribution. What do I use that is not a t-test for this?

### Wild herbivory data ----------------------------------------------------------------------
# 2019 data
Wherb19 <- read.csv("vill-all_RLB-herbivory_data_2019-Herbivory.csv", header=TRUE)
# create presence/absence of herbivory column
Wherb19 <- Wherb19 %>% mutate(pa = case_when(
  per_herbivory == 0 ~ 0,
  per_herbivory != 0 ~ 1
))

# 2020 data
Wherb22 <- read.csv("2022_wild_herbivory.csv", header = T)
# create presence/absence of herbivory column
Wherb22 <- Wherb22 %>% 
  mutate(pa = case_when(
  num_w_RLBH == 0 ~ 0,
  num_w_RLBH != 0 ~ 1
)) %>% dplyr::select(date, spp, id, pa)

## Presence absence data analysis
#2019
# Sum number of individuals with herbivory present and absent for each species for Chi squared table
present19 <- c(sum((Wherb19$pa[Wherb19$species == "alle"]) == '1'),
             sum((Wherb19$pa[Wherb19$species == "vill"]) == '1'))

absent19 <- c(sum((Wherb19$pa[Wherb19$species == "alle"]) == '0'),
            sum((Wherb19$pa[Wherb19$species == "vill"]) == '0'))

#Chi squared of presence absence 
WherbChiTable19 <- data.frame(present19, absent19)
row.names(WherbChiTable19) <- c("alle", "vill")
chisq.test(WherbChiTable19, simulate.p.value=TRUE, B=1e7)
# X-squared = 42.256, df = NA, p-value = 1e-07

#2022
# Sum number of individuals with herbivory present and absent for each species for Chi squared table
present22 <- c(sum((Wherb22$pa[Wherb22$spp == "alle"]) == '1'),
             sum((Wherb22$pa[Wherb22$spp == "vill"]) == '1'))

absent22 <- c(sum((Wherb22$pa[Wherb22$spp == "alle"]) == '0'),
            sum((Wherb22$pa[Wherb22$spp == "vill"]) == '0'))

# #Chi squared of presence absence 
# WherbChiTable22 <- data.frame(present22, absent22)
# row.names(WherbChiTable22) <- c("alle", "vill")
# chisq.test(WherbChiTable22, simulate.p.value=TRUE, B=1e7)
# # X-squared = 104.24, df = NA, p-value = 1e-07

# making combined 2019/2022 dataset for binomial regression
Wherb19$year <- '2019'
Wherb22$year <- '2022'
names(Wherb19) <- c("date","ID", "spp", "ave_per_herb", "pa", "year")
Wherb_full <- Wherb22 %>% 
  dplyr::select(spp,pa,year) %>% 
  full_join((Wherb19 %>% dplyr::select( "spp", "pa", "year")),.) 

# binomial regression 
bin_reg <- glm(Wherb_full$pa ~ Wherb_full$spp + Wherb_full$year, family = "binomial")
summary(bin_reg)
# year and species are significant predictors of the presence of herbivory at a < 0.00001 level
# because you use a binomial distribution, you should check if there is no 
# over or under-dispersion in your model. This can be tested with the package DHARMA. 
# It will also help you to check if your model adequately fits your data be investigating 
# simulated residuals. It will also flag potential outliers.

# evaluating model assumptions
# looking for over/under dispersion
simOut <- simulateResiduals(bin_reg)
plot(simOut) # dispersion looks good
testDispersion(simOut) # dispersion = 1.0241, p-value = 0.808; not overdispersed

## Percent herbivory subset
# 2019
Wherb19 <- read.csv("vill-all_RLB-herbivory_data_2019-Herbivory.csv", header=TRUE)
#  select random subset for herbivory percent analysis
set.seed(1) # use 1 to match paper
Wherb19_subset <- Wherb19 %>% 
  group_by(species) %>% 
  sample_n(10)

# assessing normality
alle_Wherb19_subset <- Wherb19_subset %>% filter(species=="alle")
shapiro.test(alle_Wherb19_subset$per_herbivory) # p-value = 0.5303 - maybe normal
hist(alle_Wherb19_subset$per_herbivory) # doesn't look very normal...
# villosissimus is all 0s, definitely not normal

# # Non-parametric test
# wilcox.test(Wherb19_subset$per_herbivory ~ Wherb19_subset$species) # W = 90, p-value = 0.0007512
# ## MEANS:  allenii - 2.54; vill - 0.00
# #t.test(Wherb19_subset$per_herbivory ~ Wherb19_subset$species) 
# # t = 3.7367, df = 9, p-value = 0.004649 - differs in signif. use wilcox as appropriate for non-normal data

# 2022
#  select random subset for herbivory percent estimation and analysis
Wherb22_subset_vill <- Wherb22 %>% filter(spp=="alle") %>%  sample_n(10)
Wherb22_subset_alle <- Wherb22 %>% filter(spp=="vill") %>%  sample_n(10)
#import herbivory estimates of subset
Wherb22_subset <- read.csv("2022_Wild_Percent_Herbivory_subset.csv", header = T)

# assessing normality
# allenii
alle_Wherb22_subset <- Wherb22_subset %>% filter(spp=="alle")
shapiro.test(alle_Wherb22_subset$ave_per_herb) # p-value = 0.244- probs normal
hist(alle_Wherb22_subset$ave_per_herb) # doesn't look very normal...
# villosissimus
vill_Wherb22_subset <- Wherb22_subset %>% filter(spp=="vill")
shapiro.test(vill_Wherb22_subset$ave_per_herb) # p=1.2689e-06 - NOT normal
hist(vill_Wherb22_subset$ave_per_herb) # mostly 0s 

# # Non-parametric test
# wilcox.test(Wherb22_subset$ave_per_herb ~ Wherb22_subset$spp) # W = 96, p-value = 0.0003801
# ## MEANS:  allenii - 3.055  ; vill - 0.1947
# t.test(Wherb22_subset$ave_per_herb ~ Wherb22_subset$spp) 
# # t = 4.873, df = 10.446, p-value = 0.0005718 -> use wilcox as appropriate for non-normal data

# 2019/2022 combined
Wherb19_subset$year <- '2019'
Wherb22_subset$year <- '2022'
names(Wherb19_subset) <- c("date","ID", "spp", "ave_per_herb", "year")
Wherb_full_subset <- Wherb22_subset %>% 
  dplyr::select(spp,ave_per_herb,year) %>% 
  full_join((Wherb19_subset %>% dplyr::select( "spp", "ave_per_herb", "year")),.)

# # Testing model assumptions
# # visual assesssment 
# lm <- lm(ave_per_herb ~ spp + year, data = Wherb_full_subset)
# par(mfrow=c(2,2)) 
# plot(lm)
# dev.off()
# summary(lm)
# #sppvill      -2.9790     0.5005  -5.952 7.28e-07 ***
# #year2022      1.2180     0.5005   2.434   0.0199 * 

# normality statistical test
shapiro.test(Wherb_full_subset$ave_per_herb) # not normal W = 0.7893, p-value = 4.084e-06
hist(Wherb_full_subset$ave_per_herb) # 0 inflated
qqp(Wherb_full_subset$ave_per_herb, "norm")

no.0 <- Wherb_full_subset %>% filter(!ave_per_herb==0)
shapiro.test(no.0$ave_per_herb) # W = 0.9706, p-value = 0.7675 - normal (we have 0 inflated gaussian)
qqp(no.0$ave_per_herb, "norm")

# # homoscedasticity statistical test
# ncvTest(lm) # Chisquare = 0.7659141, Df = 1, p = 0.38148 ; acceptable
# 
# # two way anova to look at the impact of species and year on herbivory
# aov2 <- aov(ave_per_herb ~ spp + year, data = Wherb_full_subset)
# summary(aov2) # year and species significant
# # 2 non-parametric mean comparisons. 
# wilcox.test(ave_per_herb ~ spp, data = Wherb_full_subset) #W = 358, p-value = 5.265e-06
# wilcox.test(ave_per_herb ~ year, data = Wherb_full_subset) #W = 145, p-value = 0.1151

# PERMANOVA
library('vegan')
 # method is method of pairwise distance calculation
adonis2(Wherb_full_subset$ave_per_herb ~ Wherb_full_subset$spp + Wherb_full_subset$year,
        permutations = 9999, method = "euclidean")


#### Plot wild herbivory -------------------------------------------------------------------------

# colorblind friendly options: (blue - "#868686FF" and yellow -  "#CD534CFF" or 
# pink - "#cc79a7" and yellow - "#f0e442" (better yellow than other for pairing with pink)
library(colorblindcheck)
library(ggsci)
jco_pal = pal_jco("default")(2)
palette_check(c("#cc79a7", "#f0e442", jco_pal), plot = TRUE)

# Making a simple barplot of presence/absence herbivory data from both years
spp <- c("C. allenii", "C. allenii", "C. villosissimus", "C. villosissimus")
per <- c(65.4,93.8,0,13.1)
N <- c("n=52", "n=97", "n=41", "n=61")
year <- c("2019","2022", "2019", "2022")
Wherb_PA_df <- data.frame(spp, per, N, year)
H_PA <- ggplot(data=Wherb_PA_df, aes(x=spp, y=per, fill=year)) +
  scale_fill_manual(values = c("#EFC000FF", "#0073C2FF")) +
  geom_bar(stat="identity", position = position_dodge()) +  
  ggtitle("Wild Herbivory") + 
  ylim(0, 100) +
  xlab("species")
  labs(fill='species') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('% of individuals with herbivory') + 
  scale_x_discrete(labels=c("C. allenii" = expression(italic("C. allenii")),
                            "C. villosissimus" = expression(italic("C. villosissimus")))) +
   geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)

ggsave("wild_herbivory_plot.png", device = "png", width = 10, height = 8, units = "cm")

# plot of herbivory supset data - probably not for inclusion # 
ggplot(data=Wherb_full_subset, aes(x=spp, y=ave_per_herb)) +
  geom_boxplot(aes(fill = spp)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species",
                    labels=c("alle" = expression(italic("C. allenii")),
                             "vill" = expression(italic("C. villosissimus")))) +
  theme(plot.title = element_text(hjust = 0.5), legend.text.align = 0) +
  ggtitle("Wild percent herbivory") +
  xlab("") +
  ylab("percent herbivory")+
  scale_x_discrete(labels=c("alle" = expression(italic("C. allenii")), 
                            "vill" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) +
  geom_text(x='vill', y=6, aes(label = "p<<0.0001"), size=2.5)

  
ggsave("wild_per_herbivory_plot.png", device = "png", width = 5.5, height = 7, units = "cm")


### Leaf toughness ----------------------------------------------------------------------
tough <- read.csv("leaf.toughness.csv", header = TRUE)

alle.tough <- tough[tough$species =="alle",] # subsetting alle toughness data
vill.tough <- tough[tough$species =="vill",] # subsetting vill toughness data
shapiro.test(alle.tough$ave) # not normally distributed
hist(alle.tough$ave)
shapiro.test(vill.tough$ave) # not normally distributed, but sorta close; p-value = 0.0765
hist(vill.tough$ave)

# QQ plots of different fits 
qqp(alle.tough$ave, "norm")
qqp(alle.tough$ave, "lnorm") #best 
shapiro.test(log(alle.tough$ave))
qqp(vill.tough$ave, "norm")
qqp(vill.tough$ave, "lnorm") #best 
shapiro.test(log(vill.tough$ave))

wilcox.test(log(tough$ave) ~ tough$species, paired = F) # W = 294, p-value = 0.000566
t.test(tough$ave ~ tough$species, paired = F) # t = 3.3554, df = 28.194, p-value = 0.002278
t.test(log(tough$ave) ~ tough$species, paired = F) # t = 3.6241, df = 33.24, p-value = 0.0009574

#### Plot leaf toughness --------------------------------------------------------------------
ggplot(data=tough, aes(x=species, y=ave)) +
  geom_boxplot(aes(fill = species)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species",
                    labels=c("alle" = expression(italic("C. allenii")),
                             "vill" = expression(italic("C. villosissimus")))) +
  ggtitle("Leaf Toughness") +
  theme(plot.title = element_text(hjust = 0.5), legend.text.align = 0) +
  xlab("") +
  ylab("leaf toughness (g)")+
  scale_x_discrete(labels=c("alle" = expression(italic("C. allenii")), 
                            "vill" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) +
  geom_text(x='vill', y=198, aes(label = "p<0.001"), size=2.5)

ggsave("leaf_toughness_plot.png", device = "png", width = 10, height = 7, units = "cm")

### Chemistry ----------------------------------------------------------------------------------------
# import data
flav <- read.csv('Chemistry/Flavonoids.csv', header = T)
names(flav)[5] <- "Concentration.of.Flavonoids"
sap <- read.csv('Chemistry/Saponins.csv', header = T)
phen <- read.csv('Chemistry/Phenolics.csv', header = T)

library("tidyverse")
library("car")

# combining data into one dataframe
# unrecorded steps: names(flav) to copy selected columns
chem <- flav %>% 
  dplyr::select(Number, Sample.Information, Species, Concentration.of.Flavonoids) %>% 
  left_join(sap, by = "Number", suffix = c("", ".y")) %>% 
  left_join(phen, by = "Number", suffix = c("", ".z")) %>% 
  dplyr::select(names(flav)[1:5], "Concentration.of.Saponins", "Concentration.of.Phenolics") %>% 
  mutate(Concentration.of.Saponins = round(Concentration.of.Saponins, 3), 
         Concentration.of.Phenolics_nf = Concentration.of.Phenolics - Concentration.of.Flavonoids,
         Concentration.of.Phenolics_nf = round(Concentration.of.Phenolics_nf, 3),
         Concentration.of.Flavonoids = round(Concentration.of.Flavonoids, 3))
  

# assessing normality
# dividing the data by speceis to assess within species normality
# within group normality is the assumption of an unpaired students/welches t-test
alle_chem <- chem %>% filter(Species=="ALLE")
vill_chem <- chem %>% filter(Species=="VILL")

# non-Flavonoid Phenolics 
shapiro.test(alle_chem$Concentration.of.Phenolics_nf) # W = 0.90295, p-value = 0.3492
qqp(alle_chem$Concentration.of.Phenolics_nf, "norm") 
shapiro.test(vill_chem$Concentration.of.Phenolics_nf) # W = 0.94792, p-value = 0.7107
qqp(vill_chem$Concentration.of.Phenolics_nf, "norm") 

# Flavonoids
shapiro.test(alle_chem$Concentration.of.Flavonoids) # W = 0.96981, p-value = 0.8971
qqp(alle_chem$Concentration.of.Flavonoids, "norm") 
shapiro.test(vill_chem$Concentration.of.Flavonoids) # W = 0.90093, p-value = 0.3367
qqp(vill_chem$Concentration.of.Flavonoids, "norm") 

# Saponins 
# normality
shapiro.test(alle_chem$Concentration.of.Saponins) # W = 0.86689, p-value = 0.1743
qqp(alle_chem$Concentration.of.Saponins, "norm") 
shapiro.test(vill_chem$Concentration.of.Saponins) # W = 0.85908, p-value = 0.1486
qqp(vill_chem$Concentration.of.Saponins, "norm") 

# Stats
# Phenolics
t.test(chem$Concentration.of.Phenolics_nf~chem$Species, alternative = "two.sided")
# wilcox.test(chem$Concentration.of.Phenolics_nf~chem$Species) # qualitatively equvalent to t.test
# t = 5.0223, df = 6.4727, p-value =  0.001923

# Flavonids
t.test(chem$Concentration.of.Flavonoids~chem$Species)
# wilcox.test(chem$Concentration.of.Flavonoids~chem$Species) # qualitatively equvalent to t.test
# t = -3.9792, df = 7.752, p-value = 0.004333

# Saponins
t.test(chem$Concentration.of.Saponins~chem$Species)
# wilcox.test(chem$Concentration.of.Saponins~chem$Species) # qualitatively equvalent to t.test
# t = 5.6006, df = 11.893, p-value = 0.0001199

# multiple test correction
p <- c(0.0001199, 0.004465, 0.001923)
p.adjust(p, method = "BH")
# 0.0003597 0.0044650 0.0028845
# very robust results: all still significant <0.005

#### Plot Chemistry data -------------------------------------------------------
# Saponins
 S <- ggplot(chem, aes(x=Species, y=Concentration.of.Saponins)) +
  geom_boxplot(aes(fill = Species)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species") +
  ggtitle("Saponins") + 
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") +
  ylab("Concentration of Saponins") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("ALLE" = expression(italic("C. allenii")), 
                            "VILL" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) +
  geom_text(x='VILL', y=0.99, aes(label = "p<0.001"), size=3.5) +
  theme(axis.text = element_text(size = 12))     

# Non-flavonoid Phenolics
P <- ggplot(chem, aes(x=Species, y=Concentration.of.Phenolics)) +
  geom_boxplot(aes(fill = Species)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species") +
  ggtitle("Non-Flavonoid Phenolics") + 
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") +
  ylab("Concentration of non-Flavonoid Phenolics") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels=c("ALLE" = expression(italic("C. allenii")), 
                            "VILL" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) + 
  geom_text(x='VILL', y=0.99, aes(label = "p<0.01"), size=3.5) +
  theme(axis.text = element_text(size = 12))   

# Flavonids
Fl <- ggplot(chem, aes(x=Species, y=Concentration.of.Flavonoids)) +
  geom_boxplot(aes(fill = Species)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species",
                    labels=c("ALLE" = expression(italic("C. allenii")),
                             "VILL" = expression(italic("C. villosissimus")))) +
  ggtitle("Flavonoids") + 
  theme(plot.title = element_text(hjust = 0.5), legend.text.align = 0) + 
  xlab("") +
  ylab("Concentration of Flavonoids") + 
  coord_cartesian(ylim = c(0, 0.3)) +
  guides(fill="none") +
  scale_x_discrete(labels=c("ALLE" = expression(italic("C. allenii")), 
                            "VILL" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) + 
  geom_text(x='VILL', y=0.299, aes(label = "p<0.01"), size=3.5) +
  theme(axis.text = element_text(size = 12))   
  
# Joining the plots with patchwork
library("patchwork")
S + P + Fl + plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 11.5, hjust = 0, vjust = 0))

ggsave("Chemistry_plot", device = "png", width = 24, height = 10, units = "cm")


### Beetle trials: palatability ----------------------------------------------------------------------
btd <- read.csv("beetle_trials.csv", header = TRUE) # importing beetle trial data
btd[2,1] <- 'laev'# correcting after ID confirmation
btd[7,1] <- 'guan'# correcting after ID confirmation
alleBT <- btd[btd$Species.in.trial =="alle",] # subsetting rows with alle as trial
villBT <- btd[btd$Species.in.trial =="vill",] # subsetting rows with vill as trial

alleBT$alle.mm2eaten <- as.numeric(as.character(alleBT$alle.mm2eaten))
villBT$vill.mm2eaten <- as.numeric(as.character(villBT$vill.mm2eaten))

#LMM
require(car)
require(MASS)

# Reshaping the dataset for analysis
singleSPalle <- data.frame(alleBT$Species.found.on, alleBT$Species.in.trial, alleBT$alle.mm2eaten, alleBT$date)
names(singleSPalle) <- c("species.found.on", "trial.species", "mm2.eaten", "date")
singleSPvill <- data.frame(villBT$Species.found.on, villBT$Species.in.trial, villBT$vill.mm2eaten, villBT$date)
names(singleSPvill) <- c("species.found.on", "trial.species", "mm2.eaten", "date")
singleSP <- rbind(singleSPalle, singleSPvill)

# removing 0s (8 from alle, 9 from vill) (common in insect behavior to get 0s that are thrown out unless different between treatments)
singleSP.no0 <- singleSP[singleSP$mm2.eaten != 0,]
hist(singleSP.no0$mm2.eaten)

# looking at the fit of different distributions using QQ plots - hist looks like poisson, is a sort of count: number of mm eaten. 
qqp(singleSP.no0$mm2.eaten, "norm")
qqp(singleSP.no0$mm2.eaten, "lnorm") # okay fit 
MASS::fitdistr(singleSP.no0$mm2.eaten,"Poisson")
qqPlot(singleSP.no0$mm2.eaten, distribution = "pois",lambda = 2) # okay fit 

# regression model - gaussian
gBPT  <- glmer(mm2.eaten ~ trial.species + (1 | species.found.on) + (1 | date), 
              data = singleSP.no0, 
              family = gaussian(link = "log"))  
summary(gBPT)

# regression model - poisson ## THIS IS THE BEST FIT MODEL ##
pBPT  <- glmer(mm2.eaten ~ trial.species + (1 | species.found.on) + (1 | date), 
               data = singleSP.no0, 
               family = poisson(link = "log"))  
summary(pBPT)

# evaluating model assumptions
# compare residuals
qqnorm(resid(gBPT)) 
qqline(resid(gBPT))

qqnorm(resid(pBPT)) # much better fit than lognormal
qqline(resid(pBPT))

# looking for over/under dispersion
gsimOut <- simulateResiduals(gBPT)
plot(gsimOut) # dispersion looks okay but KS test indicates wrong distribution and w/n group deviation from normality

psimOut <- simulateResiduals(pBPT)
plot(psimOut) # looks fine all around! much better fit than normal distribution
testDispersion(psimOut) # dispersion = 0.26672, p-value = 0.704

### old - AIC comparison method #####
# # simplest model for AIC comparison; no random variables 
# GHQs <- glm(mm2.eaten ~ trial.species, data = singleSP.no0,family = gaussian(link = "log"))  
# summary(GHQs) #AIC = 226.21 ; species not significant predictor of herbivory
# # # check homoscedasticity - not sure this is a thing when x is categorical with 2 values... 
# # par(mfrow=c(2,2))
# # plot(GHQs)
# # dev.off()
# 
# # test normality of data for each species
# alle.log.mm2 <- singleSP.no0 %>% filter(trial.species == "alle") %>% mutate(log.mm2.eaten = as.numeric(log.mm2.eaten)) 
# shapiro.test(alle.log.mm2$log.mm2.eaten) # p-value = 0.08872
# hist(alle.log.mm2$log.mm2.eaten) 
# vill.log.mm2 <- singleSP.no0 %>% filter(trial.species == "vill") %>% mutate(log.mm2.eaten = as.numeric(log.mm2.eaten)) 
# shapiro.test(vill.log.mm2$log.mm2.eaten) # p-value = 0.07608
# hist(vill.log.mm2$log.mm2.eaten)
# # t-test on log transformed data with 0s removed 
# singleSP.no0$log.mm2.eaten <- log(singleSP.no0$mm2.eaten)
# #t.test
# t.test(singleSP.no0$log.mm2.eaten ~ singleSP.no0$trial.species) # t = -1.1442, df = 28.831, p-value = 0.2619
# 
# # only including the date of the trial as a random variable 
# GHQ1 <- glmer(mm2.eaten ~ trial.species + (1 | date), data = singleSP.no0,family = gaussian(link = "log"))  
# summary(GHQ1) # AIC = 253.5; the date of the trial was not a significant predictor of herbivory
# 253.5-226.21 # delta AIC = 27.29 >> 2
# 
# # only including species the beetle was found on as random variable
# GHQ2 <- glmer(mm2.eaten ~ trial.species + (1 | species.found.on), data = singleSP.no0,family = gaussian(link = "log"))  
# summary(GHQ2) # AIC = 245.0; The species on which the beetle was found was not a significant predictor of herbivory
# 245.0-226.21 # delta AIC = 18.79 >> 2
# 
# # linear mixed model with date and species found on as random effects 
# GHQ3  <- glmer(mm2.eaten ~ trial.species + (1 | species.found.on) + (1 | date), data = singleSP.no0,family = gaussian(link = "log"))  
# summary(GHQ3) #AIC = 261.9; neither date nor the species on which the beetle was found are significant predictors of herbivory
# 261.9-226.21 # delta AIC = 35.69 >> 2
# 
# # Using aictab to make the comparison of models and table 
# library(AICcmodavg)
# 
# models <- list(GHQ1, GHQ2, GHQ3)
# mod.names <- c('spp.date', 'spp.sppfound', 'spp.date.sppfound')
# aictab(cand.set = models, modnames = mod.names)
# 
# # in all cases the delta AIC is way higher than 2 when compared with the simplest model
# # all models show no difference in herbivory between the species. 

### Beetle trials: choice ----------------------------------------------------------------------
btd <- read.csv("beetle_trials.csv", header = TRUE) # importing beetle trial data

# convert to long format for analysis
aOvBT.no.0$trial <- 1:40
aOvBT.no.0 <- aOvBT.no.0 %>% 
  pivot_longer(cols = c(vill.mm2eaten, alle.mm2eaten),
               names_to = "trial.spp",
               values_to = "mm2eaten")

# look at distribution of response
hist(aOvBT.no.0$mm2eaten) # likely 0 inflated poisson

# regression model
BCT  <- glmer(mm2eaten ~  trial.spp +
                (1 | trial) + 
                (1 | date) , 
              data = aOvBT.no.0, 
              family= poisson(link = "log"))  
summary(BCT)
# year and species on which the beetle was found are excluded 
# as random effects due to extremely low variance explained 
# causing a singular fit when included

# evaluating model assumptions
# check residualsxw
qqnorm(resid(BCT)) 
qqline(resid(BCT)) # not bad
# looking for over/under dispersion
simOut <- simulateResiduals(BCT)
plot(simOut) # dispersion looks good
testDispersion(simOut) # dispersion = 1.1627, p-value = 0.544
testZeroInflation(simOut) # zero inflated: ratioObsSim = 2.8004, p-value < 2.2e-16

# Fitting zero inflated model
library("glmmTMB")
ziBCT <- glmmTMB(mm2eaten ~  trial.spp +
                   (1 | trial) + 
                   (1 | date), 
                 ziformula = ~1 , 
                 family = "poisson", 
                 data = aOvBT.no.0)
summary(ziBCT)
# exp(0.3626) = 1.437061 ; so I think this means an average of 1.4 more mm2 eaten on vill...?

# evaluating model assumptions
# check residuals
plot(fitted(ziBCT), resid(ziBCT)) # characteristic fan of poisson glm
abline(h=0)
# looking for over/under dispersion
simOut <- simulateResiduals(ziBCT)
plot(simOut) # looks much better than non-zero inflated
testDispersion(simOut) # dispersion = 0.87998, p-value = 0.872
testZeroInflation(simOut) # no longer zero inflated: ratioObsSim = 0.97674, p-value = 1

# peaking at a paired t-test ignoring random effects from date
aOvBT <- btd[btd$Species.in.trial =="alle/vill",] # subsetting rows with alle and vill together as trial
aOvBT$vill.mm2eaten <- as.numeric(aOvBT$vill.mm2eaten)
aOvBT$alle.mm2eaten <- as.numeric(aOvBT$alle.mm2eaten)

# remove trials in which beetles did not eat 
aOvBT.no.0 <- aOvBT[,1:6] %>% filter(!(vill.mm2eaten == 0 & alle.mm2eaten == 0))

# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(aOvBT.no.0$vill.mm2eaten-aOvBT.no.0$alle.mm2eaten) # p-value = 0.02551
hist(aOvBT.no.0$vill.mm2eaten-aOvBT.no.0$alle.mm2eaten)
qqp(aOvBT.no.0$vill.mm2eaten-aOvBT.no.0$alle.mm2eaten, "norm")

# t-test
t.test(aOvBT.no.0$vill.mm2eaten, aOvBT.no.0$alle.mm2eaten, paired = T) # barely significant if two sided (0.04102), significant if "less", aka less on alle than vill (0.0205)
# Cohen's D
library("lsr")
cohensD(aOvBT.no.0$vill.mm2eaten, aOvBT.no.0$alle.mm2eaten, method = "paired")

#### Feeding trial plotting ----------------------------------------------------------------------
# reshape choice data long
aOvBT_long <- aOvBT.no.0 %>% 
  pivot_longer(cols = c(alle.mm2eaten, vill.mm2eaten),
               names_to = "species",
               values_to = "mm2eaten") 

# remove '.mm2eaten' from species column
aOvBT_long$species <- substr(aOvBT_long$species, 1, 4) 

#simple box plot of palatability data, shows potential for slight trend towards more vill herbivory.
palatability <- ggplot(singleSP.no0, aes(trial.species, mm2.eaten)) + 
  geom_boxplot(aes(fill = trial.species)) + 
  ggtitle("Palatability Trials") + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species") +
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('herbivory ('~mm^2*')') + 
  xlab("") +
  coord_cartesian(ylim = c(1, 43)) +
  scale_x_discrete(labels=c("alle" = expression(italic("C. allenii")), 
                            "vill" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) +
  theme(axis.text = element_text(size = 12))   

# ggsave("beetle_palatability_plot.png", device = "png", width = 5, height = 6, units = "cm")

# box plot of choice data 
choice <- ggplot(aOvBT_long, aes(species, mm2eaten)) + 
  geom_boxplot(aes(fill = species)) + 
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species",
                    labels=c("alle" = expression(italic("C. allenii")),
                             "vill" = expression(italic("C. villosissimus")))) +
  ggtitle("Choice Trials") + 
  theme(plot.title = element_text(hjust = 0.5), legend.text.align = 0) + 
  xlab("") +
  ylab("") + 
  guides(fill="none") +
  scale_x_discrete(labels=c("alle" = expression(italic("C. allenii")), 
                            "vill" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black")) + 
  geom_text(x='alle', y=33, aes(label = "p<0.05"), size=3.5) +
  theme(axis.text = element_text(size = 12))   

# plot palatability and choice feeding trials together
palatability + choice + plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 11.5, hjust = 0, vjust = 0))

ggsave("RLB_feeding_trials_plot", device = "png", width = 13, height = 7, units = "cm")
#ggsave("beetle_choice_plot", device = "png", width = 5, height = 5, units = "cm")
#ggsave("beetle_choice_plot2", device = "png", width = 9, height = 6, units = "cm")

### Herbivory arrays in intermediate habitat ----------------------------------------------------------------------
setwd("/Users/Julia/Library/CloudStorage/GoogleDrive-jharenca@ucsc.edu/My Drive/Costus/Herbivory/Herbivory_Arrays")
array <- read.csv("Herbivory_arrays_220703.csv", header = T)
names(array) <- c("ID", "array_num", "alt_ID","spp", "site", "date", "herbivory_yn", 
                  "num_RLs", "num_stms_herb", "lf1_herb", "lf1_area", "lf2_herb", "lf2_area", 
                  "lf3_herb", "lf3_area", "lf4_herb", "lf4_area", "lf5.herb", "lf5_area", 
                  "lf6.herbivory", "lf6.area", "lf7.herbivory", "lf7.area", "lf8.herbivory", 
                  "lf8.area", "lf9.herbivory", "lf9.area", "lf10.herbivory", "lf10.area", 
                  "lf11.herbivory", "lf11.area", "notes")

# Filter to only include census dates, select RL count cols, calculate proportion of rls with herbivory
array <- array %>% 
  dplyr::select(ID, array_num, spp, site, date, herbivory_yn, num_RLs, num_stms_herb) %>% # choose only certain columns to include
  filter(!is.na(num_stms_herb)) %>% # remove rows with no data in the 'num_stms_herb' column
  mutate(prop_rl_herb = num_stms_herb/num_RLs)

# regression model - beta distribution (proportion)
 
# add 0.00001 to 0 values and subtract it from 1 values to fit beta regression
array <- array %>% mutate(prop_rl_herb = case_when(
  prop_rl_herb == 0 ~ 0.00001,
  prop_rl_herb == 1 ~ 0.9999,
  TRUE ~ prop_rl_herb
))

# histogram of response
hist(array$prop_rl_herb)

# regression model
library(glmmTMB) 
AH <- glmmTMB(prop_rl_herb ~ spp + (1 | site) + (1 | date),
              data = array, 
              family=beta_family(link = "logit"))
summary(AH)  

# evaluating model assumptions
# check residuals
qqnorm(resid(AH)) 
qqline(resid(AH)) # bad
# looking for over/under dispersion
simOut <- simulateResiduals(AH)
plot(simOut) # BAD
testDispersion(simOut) # 
# regardless of model distribution, both random effects (site and date)
# explain almost no variation. The model also does not fit well 
# due to the odd distribution of the response variable,
# Attempting t-test below given that random variables appear unimportant 
# and the difference of amount eaten between species is roughly normal. 

# get proportion of rolled leaves in long format for paired t-test
dat <- array %>% pivot_wider(., id_cols=array_num, names_from = spp, values_from = prop_rl_herb)
# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(dat$alle-dat$vill) # p-value = 0.0359; close to normal
hist(dat$alle-dat$vill)
qqp(dat$alle-dat$vill, "norm")

# paired t-test
wilcox.test(dat$alle, dat$vill, paired = T) # V = 110, p-value = 0.111
t.test(dat$alle, dat$vill, paired = T) # t = 1.6954, df = 25, p-value = 0.1024

# all results are qualitatively (and close to quantitatively) the same: no difference between species. 
## NOTE - pick up in paper - explain as above - report t-test

# #################### old AIC comparison
# ## GLM delta AIC analysis to asses impact of site and array batch
# # looking at the fit of different distributions using QQ plots
# qqp(array$prop_rl_herb, "norm") # best fit... 
# shapiro.test(array$prop_rl_herb) # but very much not normal taken together... (normal within spp so fine for t-test)
# hist(array$prop_rl_herb)
# qqp(array$prop_rl_herb, "lnorm") 
# gamma <- fitdistr(array$prop_rl_herb, "gamma")
# qqp(array$prop_rl_herb, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
# 
# # looking at data
# pairs(array, pch=18)
# 
# # simplest model for AIC comparison; no random variables 
# AHs <- glm(prop_rl_herb ~ spp, data = array)  
# summary(AHs) #AIC = 48.937 ; species not significant predictor of herbivory
# 
# # only including array site as a random variable 
# AH1 <- lmer(prop_rl_herb ~ spp + (1 | site), data = array)
# summary(AH1) # 
# AIC(AH1) # 57.731
# 57.73-48.94 # delta AIC = 8.79 > 2
# 
# AH2 <- lmer(prop_rl_herb ~ spp + (1 | date), data = array)
# summary(AH2) # 
# AIC(AH2) # 57.763
# 57.76-48.94 # delta AIC = 8.82 > 2
# 
# AH3 <- lmer(prop_rl_herb ~ spp + (1 | site) + (1 | date), data = array)
# summary(AH3) # 
# AIC(AH3) # 59.73137
# 59.73-48.94 # delta AIC = 10.79 > 2
# 
# #just looking at site out of curiosity
# AHx <- glm(prop_rl_herb ~ site, data = array)
# summary(AHx) # even sig p vals are high and likely wouldn't stand up to multiple test correction
# 
# # code from above for getting k values
# library(AICcmodavg)
# 
# models <- list(AH1, AH2, AH3)
# mod.names <- c('spp.site', 'spp.batch', 'spp.site.batch')
# aictab(cand.set = models, modnames = mod.names)
# 

##### Plot Herbivory Arrays #####
ggplot(array, aes(spp, prop_rl_herb)) + 
  geom_boxplot(aes(fill=spp)) + ggtitle("Herbivory Arrays") +
  scale_fill_manual(values = c("#cc79a7", "#f0e442"), name = "Species",
                    labels=c("alle" = expression(italic("C. allenii")),
                             "vill" = expression(italic("C. villosissimus")))) +
  theme(plot.title = element_text(hjust = 0.5), legend.text.align = 0) + 
  xlab("") +
  guides(fill="none") +
  ylab("Proportion of stems with herbivory") + 
  scale_x_discrete(labels=c("alle" = expression(italic("C. allenii")), 
                            "vill" = expression(italic("C. villosissimus")))) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6, colour = "black"))

ggsave("arrays_lf_proportion_plot", device = "png", width = 6, height = 8, units = "cm")

# evaluating the subset of arrays with decent percent herbivory data
clean_subset <- read.csv('array_subset_clean_limited.csv', header = T)
all_ests <- read.csv('array_subset_all_ests.csv', header = T)
names(clean_subset) <- c("ID", "array_num", "alt_ID","spp", "site", "date", "herbivory_yn", 
                  "num_RLs", "num_stms_herb", "per_herbiv", "total_herbiv", "lf1_herb", "lf1_area", "lf2_herb", "lf2_area", 
                  "lf3_herb", "lf3_area", "lf4_herb", "lf4_area", "lf5.herb", "lf5_area", 
                  "lf6.herbivory", "lf6.area", "lf7.herbivory", "lf7.area", "lf8.herbivory", 
                  "lf8.area", "lf9.herbivory", "lf9.area", "lf10.herbivory", "lf10.area", 
                  "lf11.herbivory", "lf11.area", "notes")
names(all_ests) <- c("ID", "array_num", "alt_ID","spp", "site", "date", "herbivory_yn", 
                  "num_RLs", "num_stms_herb", "per_herbiv", "total_herbiv", "lf1_herb", "lf1_area", "lf2_herb", "lf2_area", 
                  "lf3_herb", "lf3_area", "lf4_herb", "lf4_area", "lf5.herb", "lf5_area", 
                  "lf6.herbivory", "lf6.area", "lf7.herbivory", "lf7.area", "lf8.herbivory", 
                  "lf8.area", "lf9.herbivory", "lf9.area", "lf10.herbivory", "lf10.area", 
                  "lf11.herbivory", "lf11.area", "notes")

# limited to the data with accurate values
clean_subset <- clean_subset %>% dplyr::select(array_num, spp, site, date, herbivory_yn, per_herbiv, total_herbiv) %>% filter(per_herbiv != 'na')
clean_subset$per_herbiv <- as.numeric(clean_subset$per_herbiv)
clean_subset$total_herbiv <- as.numeric(clean_subset$total_herbiv)

# Raw sneak peak -  not paired
t.test(clean_subset$per_herbiv~clean_subset$spp) # no difference
boxplot(clean_subset$per_herbiv~clean_subset$spp) # vill more varied
# Raw sneak peak - looking at only mm eaten, not as a percent
t.test(clean_subset$total_herbiv~clean_subset$spp) # alle more (p=0.025)
boxplot(clean_subset$total_herbiv~clean_subset$spp) # alle more

# paired 

# percent herbivory in long format for paired t-test
W_per_herbiv <-  clean_subset %>% pivot_wider(., id_cols=array_num, names_from = spp, values_from = per_herbiv)
# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(W_per_herbiv$alle-W_per_herbiv$vill) # p-value =  0.8337; probs normal
hist(W_per_herbiv$alle-W_per_herbiv$vill) # even looks normal
qqp(W_per_herbiv$alle-W_per_herbiv$vill, "norm")
# paired t-test
t.test(W_per_herbiv$alle, W_per_herbiv$vill, paired = T) # t = 0.54339, df = 18, p-value = 0.5935

# paired analysis of total herbivory (not as a percent)
# percent herbivory in long format for paired t-test
W_tot_herbiv <-  clean_subset %>% pivot_wider(., id_cols=array_num, names_from = spp, values_from = total_herbiv)
# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(W_tot_herbiv$alle-W_tot_herbiv$vill) # p-value =  0.3313; probs normal
hist(W_tot_herbiv$alle-W_tot_herbiv$vill) 
qqp(W_tot_herbiv$alle-W_tot_herbiv$vill, "norm")
# paired t-test
t.test(W_tot_herbiv$alle, W_tot_herbiv$vill, paired = T) # t = 4.2438, df = 18, p-value = 0.0004883


# including rough estmates
# limited to the data with accurate values
all_ests <- all_ests %>% dplyr::select(array_num, spp, site, date, herbivory_yn, per_herbiv, total_herbiv) %>% filter(per_herbiv != 'na')
all_ests$per_herbiv <- as.numeric(all_ests$per_herbiv)
all_ests$total_herbiv <- as.numeric(all_ests$total_herbiv)

# Raw sneak peak -  not paired
t.test(all_ests$per_herbiv~all_ests$spp) # no difference
boxplot(all_ests$per_herbiv~all_ests$spp) # vill more varied
# Raw sneak peak - looking at only mm eaten, not as a percent
t.test(all_ests$total_herbiv~all_ests$spp) # no difference (p=0.5389)
boxplot(all_ests$total_herbiv~all_ests$spp) 

# paired 

# percent herbivory in long format for paired t-test
W_per_herbiv <-  all_ests %>% pivot_wider(., id_cols=array_num, names_from = spp, values_from = per_herbiv)
# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(W_per_herbiv$alle-W_per_herbiv$vill) # p-value =  0.01322; not normal...
hist(W_per_herbiv$alle-W_per_herbiv$vill) # even looks normal
qqp(W_per_herbiv$alle-W_per_herbiv$vill, "norm")
# paired t-test
t.test(W_per_herbiv$alle, W_per_herbiv$vill, paired = T) # t = -0.51675, df = 23, p-value = 0.6103

# paired analysis of total herbivory (not as a percent)
# percent herbivory in long format for paired t-test
W_tot_herbiv <-  all_ests %>% pivot_wider(., id_cols=array_num, names_from = spp, values_from = total_herbiv)
# assessing whether difference is normally distributed for use of paired t-test
shapiro.test(W_tot_herbiv$alle-W_tot_herbiv$vill) # p-value = 0.06014 ; not norm
hist(W_tot_herbiv$alle-W_tot_herbiv$vill)
qqp(W_tot_herbiv$alle-W_tot_herbiv$vill, "norm")
# paired t-test
t.test(W_tot_herbiv$alle, W_tot_herbiv$vill, paired = T) # t = 0.77779, df = 23, p-value = 0.4446

## SUMMARY of subset analyses - 
# no differene in herbivory ONLY looks like more alle with the clean data, not if we include rough estimates of vill 



######## old code ###########
aOvBT$vill.mm2eaten <- as.numeric(as.character(aOvBT$vill.mm2eaten))
aOvBT$alle.mm2eaten <- as.numeric(as.character(aOvBT$alle.mm2eaten))
aOvBT.no0 <- aOvBT[(aOvBT$vill.mm2eaten != 0 | aOvBT$alle.mm2eaten != 0),] # removing trials in which beetle didnt eat at all
# write.csv(aOvBT.no0, "beetle_choice_trial.csv") # Write out to reshape dataset 
choice <- read.csv("beetle_choice_trial.csv", header = TRUE) # Read back in

# putting new dataframe in proper form
choice$Trial.number <- as.factor(choice$Trial.number)
choice$Species <- as.factor(choice$Species)
choice$date <- as.factor(choice$date)


# beetle choice trials GLMs
shapiro.test(choice$mm2eaten) # not normal
hist(choice$mm2eaten) 
hist(log(choice$mm2eaten))

#looking at QQ plots of different fits
qqp(choice$mm2eaten, "norm")
qqp(choice$mm2eaten, "lnorm") 
gamma <- fitdistr(choice$mm2eaten.1, "gamma")
qqp(choice$mm2eaten.1, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

GHQ.choice <- glmer(mm2eaten.1 ~ Species + (1 | Trial.number), data = choice, family = gaussian(link = "log")) # Linear mixed model with trial number as a random effect.
summary(GHQ.choice) # Species is significant (0.000673); AIC = 385.4

GHQ.choice2 <- glm(mm2eaten.1 ~ Species, data = choice, family = gaussian(link = "log")) # Linear mixed model without trial number as a random effect
summary(GHQ.choice2) # Species barely significant (0.0377); AIC = 307.25 so delta way more than 2
