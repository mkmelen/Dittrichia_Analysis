#### Meta Data ####


#### Packages ####
library(ggplot2)

#### Load Data ####
gh_bank_all <- read.csv("greenhouse_seedbank_data.csv")
View(gh_bank_all)

gh_bank <- gh_bank_all[gh_bank_all$SeedCount==200,]
View(gh_bank)

#### Make a compact dataframe ####
prop_means=c(mean(gh_bank$Sep1_prop), mean(gh_bank$Oct1_prop), mean(gh_bank$Nov1_prop), 
    mean(gh_bank$Dec1_prop), mean(gh_bank$Jan1_prop), mean(gh_bank$Feb1_prop),
    mean(gh_bank$Mar1_prop), mean(gh_bank$Apr1_prop), mean(gh_bank$May1_prop),
    mean(gh_bank$Jun1_prop), mean(gh_bank$Jul1_prop), mean(gh_bank$Aug1_prop),
    mean(gh_bank$Sep2_prop), mean(gh_bank$Oct2_prop), mean(gh_bank$Nov2_prop), 
    mean(gh_bank$Dec2_prop), mean(gh_bank$Jan2_prop), mean(gh_bank$Feb2_prop),
    mean(gh_bank$Mar2_prop))

months=c('Sep1','Oct1','Nov1','Dec1','Jan1','Feb1','Mar1','Apr1','May1','Jun1','Jul1','Aug1',
    'Sep2','Oct2','Nov2','Dec2','Jan2','Feb2','Mar2')

gh_bank_summary <- data.frame(prop_means, months)
View(gh_bank_summary)

gh_bank_summary$months <- ordered(gh_bank_summary$months, levels=c('Sep1','Oct1','Nov1','Dec1','Jan1','Feb1','Mar1','Apr1','May1','Jun1','Jul1','Aug1',
                                                                   'Sep2','Oct2','Nov2','Dec2','Jan2','Feb2','Mar2'))

#### Figure ####
gh_bank_gg<-ggplot(data=gh_bank_summary,
                   aes(x=months, y=prop_means)) +
  geom_line(group=1) +
  geom_point()+
  xlab("Months")+
  ylab("Proportion Germinated")+
  scale_y_continuous(limits=c(0,1),
                     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  theme_bw(base_size=16)+
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        legend.position = c(0.8, 0.4))


gh_bank_gg


#### Everything Again with non-200 values ####
#Make a Compact Dataframe
prop_means2=c(mean(gh_bank_all$Sep1_prop), mean(gh_bank_all$Oct1_prop), mean(gh_bank_all$Nov1_prop), 
             mean(gh_bank_all$Dec1_prop), mean(gh_bank_all$Jan1_prop), mean(gh_bank_all$Feb1_prop),
             mean(gh_bank_all$Mar1_prop), mean(gh_bank_all$Apr1_prop), mean(gh_bank_all$May1_prop),
             mean(gh_bank_all$Jun1_prop), mean(gh_bank_all$Jul1_prop), mean(gh_bank_all$Aug1_prop),
             mean(gh_bank_all$Sep2_prop), mean(gh_bank_all$Oct2_prop), mean(gh_bank_all$Nov2_prop), 
             mean(gh_bank_all$Dec2_prop), mean(gh_bank_all$Jan2_prop), mean(gh_bank_all$Feb2_prop),
             mean(gh_bank_all$Mar2_prop))

months2=c('Sep1','Oct1','Nov1','Dec1','Jan1','Feb1','Mar1','Apr1','May1','Jun1','Jul1','Aug1',
         'Sep2','Oct2','Nov2','Dec2','Jan2','Feb2','Mar2')

gh_bank_summary2 <- data.frame(prop_means2, months2)
View(gh_bank_summary2)

gh_bank_summary2$months2 <- ordered(gh_bank_summary2$months2, levels=c('Sep1','Oct1','Nov1','Dec1','Jan1','Feb1','Mar1','Apr1','May1','Jun1','Jul1','Aug1',
                                                                   'Sep2','Oct2','Nov2','Dec2','Jan2','Feb2','Mar2'))
# Figure

gh_bank_gg2<-ggplot(data=gh_bank_summary2,
                   aes(x=months2, y=prop_means2)) +
  geom_line(group=1) +
  geom_point()+
  xlab("Months")+
  ylab("Proportion Germinated")+
  scale_y_continuous(limits=c(0,1),
                     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  theme_bw(base_size=16)+
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        legend.position = c(0.8, 0.4))


gh_bank_gg2





