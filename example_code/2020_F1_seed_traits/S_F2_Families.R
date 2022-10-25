#defining directory
dir='/Users/Miranda/Documents/Education/R\ Directory/DIGR/2020_F1_seed_traits'
#setting directory
setwd(dir)
#loading file as a dataframe
seeds <-as.data.frame(read.csv("D_F2_Families.csv"))
#subsetting to keep rep=B
seeds<-(subset(seeds,rep=="B"))
#subsetting to keep measure_traits=no
seeds<-(subset(seeds,measure_traits=="no"))
#droping unused levels
seeds$pop<-droplevels(seeds$pop)
#printing current levels (unique categories)
levels(seeds$pop)
