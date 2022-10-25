### DITTRICHIA DATA ####

# load data
digr_vegtable <- read.csv("DIGRsurveys_abundance.csv", header=T, stringsAsFactors = T)
  str(digr_vegtable)
digr_plots <- read.csv("plots_Andrew.csv", header=T, stringsAsFactors = T)
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
plot(digr_axis1,digr_axis2,pch=16, col=factor(digr_plots$plot_type)) # roads are red
text(digr_axis1+0.01,digr_axis2+0.01,digr_plots$plot_type,cex=0.8)
  
# Calculate the fraction of variation represented by the first two axes, and scale the ordination accordingly
c(100*digr_PCO$values/sum(digr_PCO$values))[1:2] # this is the percentage of variance on the first and second axis --> write down below
plot(digr_axis1,digr_axis2,xlab="Axis 1 (20% of variance)", ylab="Axis 2 (13% of variance)",pch=as.character(digr_plots$plot_type),cex=0.9,xlim=c(-0.4,0.4),ylim=c(-0.4,0.4))
  
#roads.arrow <- vf(digr_PCO$vectors[,1:2],as.factor(digr_plots$plot_type),nperm=0)
digr_species.arrows <- vf(digr_PCO$vectors[,1:2],digr_vegmatrix,nperm=0)
#digr_cover.arrow <- vf(digr_PCO$vectors[,1:2],digr_plots$digr_cover,nperm=0) # once you add this column you should be able to plot arrows indicating DIGR abundance
  
# Calculate correlations between the first two axes and individual species or soil moisture and draw a graph with arrows
plot(digr_axis1,digr_axis2,xlab="Axis 1 (20% of variance)", ylab="Axis 2 (13% of vaiance)",pch=16,col=factor(digr_plots$plot_type),xlim=c(-0.4,0.4),ylim=c(-0.4,0.4))
plot.vf(digr_species.arrows[digr_species.arrows[,"r"]>0.6,]) # change the number 0.6 to get more or fewer arrows

