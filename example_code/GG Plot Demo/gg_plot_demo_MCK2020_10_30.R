#Matt Kustra 10/29/20
#Run the line below by removing the hashtag if you haven't already installed tidyverse
#install.packages("tidyverse")
#install.packages("palmerpenguins")

#this loads the tidyverse package which includes ggplot
library(tidyverse)
#this will load the dataset I will be working on
library(palmerpenguins)
#Useful websites for reference (From Tanya Rogers):
#http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html?platform=hootsuite
#http://www.cookbook-r.com/Graphs/
#http://docs.ggplot2.org/current/
#https://r4ds.had.co.nz/
#https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#ggplot relies on data frames,luckily ggplot package includes a few data sets that we can use for practice so lets load up a data set
#For this tutorial we will use the penguin dataset from the package palmerpenguins
Data<-penguins
#Ask for help by starting with a ?
?penguins()
#first lets see what is in our example data, 
str(Data)
head(Data)
#now lets try ggplot
#First argument "data = Data" is the data frame that has all the data you want to plot
#the next argument "aes(....)" defines mappings of the plots.... so for example aes (x = var1, y = var2) 
#will make the X axis be values from the column var1, values from the column var2 will make up y values. 
#These mappings are default for all geom's but you can overwrite them by definining aes values inside specific geom's

#lets try plotting how Bodywt on the X axis and brainwt on the Y axis. 
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm))
#It looks empty, that's because we haven't actually specified a "geom" for ggplot to plot. 
#for a basic scatter plot we just specify + geom_point()
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm))+geom_point()
#Looks like we have three different clusters
#looking back at the data set "str(Data)" we have multiple species, so let's assign them a different color
#So to help us look at that differently we can add something to the aes() mappings
#Let's try adding color=Treatment
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species))+geom_point()
#Let's try adding shape=Treatment
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species,shape=island))+geom_point()

#Now the cool thing with GGplot is it's very easy to add more layers and the way we do that is with + geom_...() 
#So on top of what we already have lets add geom_smooth() which fits a curve to your data...The gray shading around 
#it represents the 95% CI...the default is using "loess" function which is locally fitting a polynomial.
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species))+
  geom_point()+ #geom = geometries... are you making points, lines, bars?
  geom_smooth() #another layer

#Geom's have their own parameters you can play around with for example if we to fit a line instead of a curve we can add method="lm"
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species))+geom_point()+geom_smooth(method="lm")
#With the geoms it's important to note that the order in which you add geoms determines the order that that they are layered on one another...
#For example lets swap the geom_point() and geom_smooth
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species))+geom_smooth(method="lm")+geom_point()
#There are alot of other factors in the data set not considered that we could try and visualize
#One that comes to mind is"Sex"
#We can make colors represent different factor combinations using interaction()
#e.g. interaction(Var1, Var2,...,VarN)
ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=interaction(species,sex)))+geom_smooth(method="lm")+geom_point()
#Oops there are some NA's in the sex column which is causing it to go bonkers...Let's quickly remove those.
#you can subset your data within ggplot which is nice
ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=interaction(species,sex)))+geom_smooth(method="lm")+geom_point()
#And there is a lot going on, which we don't like and that's way to busy of a figure...So we can break it down in a more digestible manner using facet_wrap() or facet_grid()
#facet_grid(.~categorical column) stacks it all in one column with different rows for each categorical column
ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=sex))+geom_smooth(method="lm")+geom_point()+facet_grid(.~species)
#facet_grid(categorical column~.) Makes graphs all in one row with different columns for each categorical column
ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=sex))+geom_smooth(method="lm")+geom_point()+facet_grid(species~.)
#alternatively since we have two factors we can do a grid
#facet_grid(X_categorical column ~ y_Categorical Column)
ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=sex))+geom_smooth(method="lm")+geom_point()+facet_grid(species~island)
#While we are on facet things, you can also facet by more than two with "+" like: facet_grid(cat1~cat2+cat3)
ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=sex))+geom_smooth(method="lm")+geom_point()+facet_grid(species+year~island)
###Lets rewind and go back to our basic plot to talk about a few things 
#another good thing about ggplot is we can save it as a variable
bill_plot<-ggplot(data=Data,aes(x=bill_length_mm,y=bill_depth_mm,color=species))+geom_point()+geom_smooth(method="lm",show.legend = F)
#to plot it 
bill_plot
##say we want to remove a layer from the plot, 
#first lets see what the layers are
bill_plot$layers
#to remove a layer, in this case lets remove the smooth fits which is layer 2
bill_plot$layers[[2]]<-NULL
#see if it worked
bill_plot
#Another great thing about ggplot is that we can just add layers
bill_plot+geom_smooth(method="lm")
###Let's try to make this plot more "publishable"/nicer
##Fixing axis labels
#Use +xlab("label") and +ylab("label") to change axis labels 
bill_plot<-bill_plot+geom_smooth(method="lm",show.legend = FALSE)+xlab("Bill length (mm)") + ylab("Bill depth (mm)")
bill_plot
##Themes
#I personally do not like ggplot base theme. One that I like is theme_classic(), theres a package called "ggthemes" that have even more.
#to use one of these themes just add it like any other layer.
bill_plot+theme_classic()
#I still like to make changes such as text size among other things, so you can really customize it how you want using theme()
#lets look at all the different things you can customize.
?theme()
#It takes a bit to figure out how to do specific things, Google and stackoverflow are your friends!
bill_plot+theme_classic()+theme(legend.position ="bottom",#this puts legend on the bottom
                                axis.title=(element_text(face="bold")),#this makes the axis titles in bold,
                                axis.line=element_line(color="black",size=2),#Makes the axis line black and thicker
                                text=element_text(size=15,face="bold"))#makes all the text larger and bold
#So you don't have to keep typing those changes all the time...you can save your theme as a variable to use
mytheme<-theme_classic()+theme(legend.position ="bottom",#this puts legend on the bottom
                               axis.title = (element_text(face="bold")),#this makes the axis titles in bold,
                               axis.line=element_line(color="black",size=2),#Makes the axis line black and  thicker
                               text=element_text(size=15,face="bold"))#makes all the text larger and bold
#and to confirm that it worked...
bill_plot+mytheme
#you can change the axis labels
bill_plot+mytheme+xlab("Bill length (mm)")+ylab("Bill depth (mm)")
#you can also set the default theme to your theme 
theme_set(mytheme)
bill_plot 
?theme_set()
#also to change a specific thing just +theme(do whatever)
#For example say I want red axis line, but want to keep everything else I changed the same
bill_plot+theme(axis.line=element_line(color="red",size=2))
##Changing default colors using scale_color_manual, change legend title with name, "values" is the colors you want to use, "breaks" are the names in your data frame, "labels" is what you want showed; this defaults to breaks
#here is a website with good color values http://sape.inf.usi.ch/quick-reference/ggplot2/colour
bill_plot+scale_color_manual(name="Species",values=c("orchid1","rosybrown2","seagreen2"))
Figure1<-bill_plot+scale_color_manual(name="Species",values=c("orchid1","rosybrown2","seagreen2"))
####Set working directory to save some of our plots
setwd("~/Documents/Education/R Directory/GG Plot Demo")
#ggsave by default will save your current plot, you can also specify which plot you want to save (make sure to make your plot a variable)
#supply width and height, what units those measures are, and for rasters you supply dpi
ggsave("Figure1_lowres.png",Figure1,width = 6,height=4,units="in",dpi=150)
ggsave("Figure1_highres.png",Figure1,width = 6,height=4,units="in",dpi=500)
#will automatically know what file to export it as based on extension...this saves as a pdf and because it's vector based you don't need the dpi...this is probably best way to save final figures that most journals will ask for.
ggsave("Figure1.pdf",Figure1,width = 6,height=4,units="in")

###Other kinds of plots...
#So we did scatterplots which show us how two different continuous variables relate to one another across different parameters...but let's try just focusing on one variable bod
#We can make histograms. Use a different color for the species (color= line of column, whereas "fill"= the column color).
ggplot(data=Data,aes(x=flipper_length_mm,color=species))+geom_histogram()
#oh that looks wierd..that's because for many geoms (e.g. histogram,box_plot,violin_plot) color is the lines/borders, what we actually wnat are the inside which is fill
ggplot(data=Data,aes(x=flipper_length_mm,fill=species))+geom_histogram()
#I think these colors are too "strong" so I like to make it more transparent...via alpha setting
#also make a border by setting color. 
#If we set these things outside of aes() they apply to everything
ggplot(data=Data,aes(x=flipper_length_mm,fill=species))+geom_histogram(alpha=0.4,color="black")

#Now lets try a box plot...unlike histogram which requires only an x geom_boxplot requires a y
ggplot(data=Data,aes(x=species,y=flipper_length_mm,fill=species))+geom_boxplot(alpha=0.4)
#We can add our actual datapoints to this with geom_jitter() which spaces the data points out
ggplot(data=Data,aes(x=species,y=flipper_length_mm,fill=species))+geom_boxplot(alpha=0.4)+geom_jitter()
#little distracting so lets make them transparent
ggplot(data=Data,aes(x=species,y=flipper_length_mm,fill=species))+geom_boxplot(alpha=0.4)+geom_jitter(alpha=0.2)
#Going back to our data we have 3 other continous variables to go through...We could copy and paste this and just change the y variable...or We can make a function!!

##Box plots
ggplot(data=Data,aes(x=species,y=flipper_length_mm,fill=species))+
         geom_jitter(alpha=0.4)+
         geom_boxplot()

#We need to use aes_string instead of aes...This is because aes using column names which are not strings
box_plot_func<-function(data,xvar,yvar,xlabel=xvar,ylabel=yvar,fillvar,fillLabel=fillvar){
  ggplot(data=data,aes_string(x=xvar,y=yvar,fill=fillvar))+geom_boxplot(alpha=0.4)+ylab(ylabel)+xlab(xlabel)+labs(fill=fillLabel)+geom_jitter(alpha=0.2)
}
#now let's try it
f<-box_plot_func(Data,"species","flipper_length_mm","Species","Flipper Length (mm)",fillvar="species")
bl<-box_plot_func(Data,"species","bill_length_mm","Species","Bill Length (mm)",fillvar="species")
bd<-box_plot_func(Data,"species","bill_depth_mm","Species","Bill Depth (mm)",fillvar="species")
#putting graphs together
#install.packages("patchwork")
library(patchwork)
#better/more in depth tutorial online
#essential "|" or "+" will put it side to side "/" will put it underneath 
allplots_raw<-(bill_plot|bl)/(bd|f)
allplots_raw
#So some issues: (1) legends are redundent and (2) we want to label the subplots
#lets add subplot labels to the plots using plot_annotation
#tag_levels="A" give capital lettering, "a" gives lowercase, "I" is uppercase roman numerals, "i" is lowercase roman numerals 
allplots_raw+plot_annotation(tag_levels = "A") #or "a" does lowercase and "i" does Roman numerals
#to fix the legend we can use plot_layout
allplots_raw+plot_annotation(tag_levels = "A")+plot_layout(guides="collect")
#still redundency,best to go back to old plots and edit them
#remove legends from boxplots
f<-f+theme(legend.position = "none")
bl<-bl+theme(legend.position = "none")
bd<-bd+theme(legend.position = "none")
allplots<-(bill_plot|bl)/(bd|f)+plot_annotation(tag_levels = "A")+plot_layout(guides="collect")
#want to change the circles to solid squres use this 
bill_plot<-bill_plot+ guides(color = guide_legend(override.aes = list(size =5,shape="square")))
allplots<-(bill_plot|bl)/(bd|f)+plot_annotation(tag_levels = "A")+plot_layout(guides="collect")
allplots

#Other quick example using patchwork with one large panel and three smaller panels
by_island<-ggplot(data=Data[!is.na(Data$sex),],aes(x=bill_length_mm,y=bill_depth_mm,color=species))+geom_smooth(method="lm",show.legend = F)+geom_point()+facet_grid(.~island)+ guides(color = guide_legend(override.aes = list(size =5,shape="square")))+xlab("Bill length MM") +ylab('Bill depth mm')
plots_large_bottom<-(f+bl+bd)/by_island+plot_annotation(tag_levels = "A")
ggsave("multi-panel.png",plots_large_bottom,width = 10,height=8,units="in",dpi=500)


###moving on to different things, sometimes we want to plot mathematical functions, which can be a little hard in ggplot since it requires a data frame...One way to do this is by making a for loop to generate a data fram but that is not ideal...Here is a much better way.
#Say we want to plot a function like
logistic_growth<-function(K,r,x,N0){
  return(K/(1+(K-N0/N0)*exp(-r*x)))
}
#ggplot(data=data.frame(x=0),mapping=aes(x=x)) this part tricks it 
ggplot(data=data.frame(x=0),mapping=aes(x=x))+xlab("Time")+ylab("Population Size")+xlim(0,1000)+stat_function(fun=logistic_growth,args=(list(K=2000,r=0.015,N0=10)),size=1.5,mapping=aes(color="K = 2000"))+stat_function(fun=logistic_growth,args=(list(K=1500,r=0.015,N0=10)),mapping=aes(color="K = 1500"),size=1.5)+labs(color="Carrying Capacities")


