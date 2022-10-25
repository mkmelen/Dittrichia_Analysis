### DITTRICHIA DATA ####

# load data
digr_vegtable <- read.csv("DIGRsurveys_presence_absence.csv", header=T, stringsAsFactors = T)
str(digr_vegtable)
digr_plots <- read.csv("plots_Andrew.csv", header=T, stringsAsFactors = T)
  digr_plots$pair_nr <- as.factor(digr_plots$pair_nr)
  str(digr_plots)

# Transform the data frame with species abundances into a matrix 
digr_vegmatrix <- data.matrix(digr_vegtable[,-1])
rownames(digr_vegmatrix) <- digr_vegtable$Species

# Eliminate rare species (found only in 1–2 plots) and transpose the matrix
digr_freq <- apply(digr_vegmatrix,1,function(x) sum(sign(x)))
digr_vegmatrix <- digr_vegmatrix[digr_freq>2,]
digr_vegmatrix <- t(digr_vegmatrix)
digr_vegmatrix  

# Distance matrix between all pairs of plots
library(ecodist)
digr_DM <- distance(digr_vegmatrix,method="bray")
  
# Principal coordinates analysis
digr_PCO <- pco(digr_DM)
digr_axis1 <- digr_PCO$vectors[,1] * digr_PCO$values[1] 
digr_axis2 <- digr_PCO$vectors[,2] * digr_PCO$values[2]
  
# Simple ordination
#plot(digr_axis1,digr_axis2,pch=16, col=factor(digr_plots$plot_type)) # roads are red
#text(digr_axis1+0.01,digr_axis2+0.01,digr_plots$pair_nr,cex=0.8)
  
# Calculate the fraction of variation represented by the first two axes, and scale the ordination accordingly
#c(100*digr_PCO$values/sum(digr_PCO$values))[1:5] # this is the percentage of variance on the first and second axis --> write down below
#plot(digr_axis1,digr_axis2,xlab="Axis 1 (22% of variance)", ylab="Axis 2 (15% of variance)",pch=as.character(digr_plots$plot_type),cex=0.9,xlim=c(-0.4,0.4),ylim=c(-0.4,0.4))
  
#roads.arrow <- vf(digr_PCO$vectors[,1:2],as.factor(digr_plots$plot_type),nperm=0)
digr_species.arrows <- vf(digr_PCO$vectors[,1:2],digr_vegmatrix,nperm=0)
  digr_species.arrows 

digr_cover.arrow <- vf(digr_PCO$vectors[,1:2],digr_plots$coverDIGR,nperm=0) 
  rownames(digr_cover.arrow) <- "Dittrichia cover"
veg_cover.arrow <- vf(digr_PCO$vectors[,1:2],digr_plots$coverVeg,nperm=0) 
  rownames(veg_cover.arrow) <- "Vegetation"
bare_cover.arrow <- vf(digr_PCO$vectors[,1:2],digr_plots$coverBare,nperm=0) 
  rownames(bare_cover.arrow) <- "Bare ground"

popsize.arrow <- vf(digr_PCO$vectors[,1:2],digr_plots$popsize,nperm=1000) 
  rownames(popsize.arrow) <- "Dittrichia pop size"
  
### PLOT ###
  
# this is where you pick your colors:
palette(c("black","chartreuse4")) # first color for offroad, second for road

plot(digr_axis1,digr_axis2,xlab="Axis 1 (22% of variance)", ylab="Axis 2 (15% of variance)",pch=16,col=factor(digr_plots$plot_type),xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), cex=log(digr_plots$popsize)/3) # point size by DIGR pop size

plot.vf(digr_species.arrows[digr_species.arrows[,"r"]>0.6,], cex=.8) # change the number 0.6 to get more or fewer arrows

plot.vf(digr_cover.arrow, col="goldenrod2", lwd=3, cex=1.3)
plot.vf(veg_cover.arrow, col="darkgreen", lwd=3, cex=1.3)
plot.vf(bare_cover.arrow, col="burlywood3", lwd=3, cex=1.3)

plot.vf(popsize.arrow, col="goldenrod2", lwd=3) 



