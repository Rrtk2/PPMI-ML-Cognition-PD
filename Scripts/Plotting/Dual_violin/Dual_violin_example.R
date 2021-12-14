#-----------------------------------------------------------------------------------------------------#
#							ggplot dual boxplot generation
#-----------------------------------------------------------------------------------------------------#

# devtools::install_github('erocoar/gghalves')
library(gghalves)
library(ggplot2)
library(cowplot)

a=data.frame(a1=1:100,
a2=c(rnorm(99,10,1),15), 
b=c(rnorm(99,12,1),100),
d=c(rep(0,50),rep(1,50)))


dat1 = a[a$d==0,]
dat2 = a[a$d==1,]

s_violin_width = 0.3
s_boxplot_width = s_violin_width/4
s_violin_alpha = 0.3

# get y ranges
Yrange = range(c(dat1$a2,dat2$a2))

# make figure
box3 <- ggplot(data=dat1, aes(x=1, y=a2)) +
	# right (up) part
	geom_half_violin(side=c("r"),alpha = s_violin_alpha,width = s_violin_width, fill = "green2")+
	geom_half_boxplot(side=c("r"),width = s_boxplot_width, lwd = 1,fill = "green2",outlier.colour = "green4",errorbar.length=1)+
	# left (down) part 
	geom_half_violin(data=dat2,side=c("l"),alpha = s_violin_alpha, width = s_violin_width,fill = "red2")+
	geom_half_boxplot(data=dat2,side=c("l"),width = s_boxplot_width, lwd = 1,fill = "red2",outlier.colour = "red4",errorbar.length=1)+
	# additionals
	ylim(Yrange[1]*0.95, Yrange[2]*1.05)+
	coord_flip()+
	theme_cowplot()+
	theme(legend.position = "none")+
	theme(axis.ticks.y=element_blank())+
	theme(axis.text.y=element_blank())+
	ggtitle(paste("Temp_example"))+
	#ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
	ylab("")+
	xlab("")+
	scale_fill_manual(values = c("red2","green2"))+
	scale_color_manual(values = c("red3","green3"))# makes the contrast slightly better


# plot figure
box3



#-----------------------------------------------------------------------------------------------------#
#							Prepare for plot generation in ML script
#-----------------------------------------------------------------------------------------------------#

# This is for 1 data fragment, to do this for all 2*4*4, repeat as in the PlotMultiROCv2.R script
# this is not the dedicated plotter for the exact image we want to generate, just to get it to work and experiment!

# load data example
load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/All/cforest_image.RData")

# uses dataraw_pheno_new (data raw) & df (top importance list and such)
#dataraw_pheno_new

dat1 = dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",]
dat2 = dataraw_pheno_new[dataraw_pheno_new$Class=="Dementia \nConverter ",]

# settings plot
s_violin_width = 0.3
s_boxplot_width = s_violin_width/5
s_violin_alpha = 0.3
s_nudge = 0.000


plot_list <- list()

for(x in 1:tophitsamount){
	if(df[x,"Type"] == "CON"){
			  
		# get y ranges
		Yrange = range(c(dataraw_pheno_new[,as.character(df$Names[x])]))
	  

		# make figure
		plot_list[[x]] <- ggplot(data=dat1, aes_string(x=1, y=as.character(df$Names[x]),fill="Class",color="Class")) +
			# right (up) part
			geom_half_violin(side=c("r"),alpha = s_violin_alpha,width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(side=c("r"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "green4",errorbar.length=1)+
			# left (down) part 
			geom_half_violin(data=dat2,side=c("l"),alpha = s_violin_alpha, width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(data=dat2,side=c("l"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "red4",errorbar.length=1)+
			# additionals
			ylim(Yrange[1]*0.95, Yrange[2]*1.05)+
			coord_flip()+
			theme_cowplot()+
			theme(legend.position = "none")+
			theme(axis.ticks.y=element_blank())+
			theme(axis.text.y=element_blank())+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
			ylab("")+
			xlab("")+
			scale_fill_manual(values = c("red2","green2"))+
			scale_color_manual(values = c("red3","green3"))# makes the contrast slightly better


	}else{
			
		dataraw_pheno_new[,as.character(df$Names[x])] <- as.factor(dataraw_pheno_new[,as.character(df$Names[x])])

		plot_list[[x]]  <- ggplot(dataraw_pheno_new, aes_string(fill=as.character(df$Names[x]), y="count", x="Class")) + 
			geom_bar(position="fill", stat="identity")+
			coord_flip()+
			ggtitle("")  +
			xlab("")+
			ylab("")+
			theme_cowplot()+
			scale_fill_brewer()+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))
	}
}

for(i in 1:length(plot_list)){
	assign(paste0("plot_",i),value = plot_list[[i]])
}


# illustrate how it looks
#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,
#	  plot_9,plot_10,nrow = 5,ncol = 2)


#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,nrow = 5,ncol = 2)

g_plot = plot_grid(plotlist = plot_list,nrow = ceiling(tophitsamount/2),ncol = 2)

#g_plot = plot_grid(plotlist = plot_list,ncol = 1)

# Save
if(s_Saving){
	png(paste0("Group_differences.png"),width = 4000,height = 6700, res = 350)
		plot(g_plot)
	dev.off()          
}

#-----------------------------------------------------------------------------------------------------#
#							geom bar test
#-----------------------------------------------------------------------------------------------------#
library(gghalves)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)


# load data example
load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/Clinical/rf_image.RData")
#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/All/cforest_image.RData")

tophitsmaxamount = 1000 # big number means all
tophitsamount = min(dim(superresult$Combine_data_main$model[[1]]$importance$importance)[1],tophitsmaxamount)
#dataoptions = "Combine_data_main"

# check if varimp already exists else remove
if(exists("temp_Varimp")){rm("temp_Varimp")}

#temp_data = get(dataoptions)
temp_Varimp = superresult$Combine_data_main$model[[1]]$importance$importance

rownames_variimp = rownames(temp_Varimp)

		
ReorderID = NULL
ReorderID = order(temp_Varimp[[1]],decreasing = T) #@RRRnote: [[1]] means class0

if(is.null(ReorderID)){
	temp_Varimp = temp_Varimp[[1]] #@RRRnote: [[1]] means class0
	ReorderID = order(temp_Varimp,decreasing = T)
	temp_Varimp = data.frame(temp_Varimp[ReorderID])
}else{
	temp_Varimp = data.frame(temp_Varimp[ReorderID,1]) # selected col 1 -> means class 0
}




rownames(temp_Varimp) = rownames_variimp[ReorderID]
colnames(temp_Varimp) = "VarImp"



df = data.frame(
  Names=rownames(temp_Varimp)[1:tophitsamount],
  VarImp=temp_Varimp[1:tophitsamount,]
)

# Add additional data labels from the feature labels file
# Add variable information
df$Type <- NULL
#df_Full_Label <- NULL
for(x in df$Names){
  df[which(df$Names == x),"Type"] <- dataraw_feature[which(dataraw_feature$Label == x),"Type"]
  df[which(df$Names == x),"Full_Label"] <- dataraw_feature[which(dataraw_feature$Label == x),"Full_Label"]
}

# Re-load the raw pheno file and subset relevant groups
dataraw_pheno_new = read.csv("file://C:/DATA STORAGE/Projects/PPMI/Data RAW/Baseline_EpigeneticsAdded_140721.csv", header=TRUE, stringsAsFactors=FALSE)


# make classes by defined class	of interest
if(s_Lancet_classes==0){
	dataraw_pheno_new$PDD_Group = NA

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Normal")] <- 0#0
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "MCI")] <- 1#1
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Dementia")] <- 1#2
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
}


if(s_Lancet_classes==1){
	# Load alternative classification based on lancer paper
	lancet_class = data.frame(data.table::fread(Lancetdata, header=TRUE, stringsAsFactors=FALSE)) 

	dataraw_pheno_new$Lancet_Class = lancet_class$BinaryPDD
	dataraw_pheno_new$PDD_Group = dataraw_pheno_new$Lancet_Class

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "NonDementia")] <- 0#0
	
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "Dementia")] <- 1#0
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
	
}



#dataraw_pheno_new[which(dataraw_pheno_new$Conversion_group == "Normal_PDD" |dataraw_pheno_new$Conversion_group == "MCI_PDD" ),"PDD_Group"] <- 1
dataraw_pheno_new <- dataraw_pheno_new[-which(is.na(dataraw_pheno_new$PDD_Group)),]
dataraw_pheno_new$Class <- as.factor(dataraw_pheno_new$PDD_Group)
levels(dataraw_pheno_new$Class) <- c("Stable","Dementia \nConverter ")
dataraw_pheno_new$count <-  1 # Count variable for stacked bar charts



# uses dataraw_pheno_new (data raw) & df (top importance list and such)
#dataraw_pheno_new

dat1 = dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",]
dat2 = dataraw_pheno_new[dataraw_pheno_new$Class=="Dementia \nConverter ",]

# settings plot
s_violin_width = 0.3
s_boxplot_width = s_violin_width/5
s_violin_alpha = 0.3
s_nudge = 0.000

scale_fill_brewer(palette = "Reds")

plot_list <- list()

x=8

# bump up somewhere in real code:
dataraw_pheno_new$ClassInvPrio = factor(dataraw_pheno_new$Class,levels =c( "Dementia \nConverter ","Stable"))

#start
dataraw_pheno_new[,as.character(df$Names[x])] <- as.factor(dataraw_pheno_new[,as.character(df$Names[x])])

ncolors = length(unique(dataraw_pheno_new[,as.character(df$Names[x])]))
Reds = RColorBrewer ::brewer.pal(ncolors, "Reds")
Greens = RColorBrewer ::brewer.pal(ncolors, "Greens")
Grays = RColorBrewer ::brewer.pal(ncolors, "Greys")

Colorlist = c(Greens,Reds)

Raw_plot = ggplot(dataraw_pheno_new, aes_string(x="ClassInvPrio",y="count", fill=paste0("Class:",as.character(df$Names[x])) )) + 
			geom_bar(position="fill",stat="identity")+
			coord_flip()+
			ggtitle("")+
			xlab("")+
			ylab("")+
			theme_cowplot()+
			#scale_fill_manual(values = c("red2","green2","green3"))+
			#scale_fill_brewer(palette = "Reds")+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
			scale_fill_manual(values = Colorlist)+
			theme(legend.position="none")
			
			
			
			
legend_plot = ggplot(dataraw_pheno_new, aes_string(fill=paste0(as.character(df$Names[x])), y="count", x="Class")) + 
			geom_bar(position="fill", stat="identity")+
			coord_flip()+
			ggtitle("")  +
			xlab("")+
			ylab("")+
			theme_cowplot()+
			#scale_fill_manual(values = c("red2","green2","green3"))+
			#scale_fill_brewer(palette = "Reds")+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
			scale_fill_manual(values = Grays)
			
			
			
# Extract legend from the legend_plot
tmp <- ggplot_gtable(ggplot_build(legend_plot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
mylegend <- tmp$grobs[[leg]]


# add in legend to the right
plot_list[[x]] = grid.arrange(arrangeGrob(Raw_plot),mylegend,ncol=2,widths=c(8, 2),newpage=TRUE)

#end

#-----------------------------------------------------------------------------------------------------#
#							Generation of MORE features (10+)
#-----------------------------------------------------------------------------------------------------#

# load data example
load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/Clinical/rf_image.RData")
#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/All/cforest_image.RData")

tophitsmaxamount = 1000 # big number means all
tophitsamount = min(dim(superresult$Combine_data_main$model[[1]]$importance$importance)[1],tophitsmaxamount)
#dataoptions = "Combine_data_main"

# check if varimp already exists else remove
if(exists("temp_Varimp")){rm("temp_Varimp")}

#temp_data = get(dataoptions)
temp_Varimp = superresult$Combine_data_main$model[[1]]$importance$importance

rownames_variimp = rownames(temp_Varimp)

		
ReorderID = NULL
ReorderID = order(temp_Varimp[[1]],decreasing = T) #@RRRnote: [[1]] means class0

if(is.null(ReorderID)){
	temp_Varimp = temp_Varimp[[1]] #@RRRnote: [[1]] means class0
	ReorderID = order(temp_Varimp,decreasing = T)
	temp_Varimp = data.frame(temp_Varimp[ReorderID])
}else{
	temp_Varimp = data.frame(temp_Varimp[ReorderID,1]) # selected col 1 -> means class 0
}




rownames(temp_Varimp) = rownames_variimp[ReorderID]
colnames(temp_Varimp) = "VarImp"



df = data.frame(
  Names=rownames(temp_Varimp)[1:tophitsamount],
  VarImp=temp_Varimp[1:tophitsamount,]
)

# Add additional data labels from the feature labels file
# Add variable information
df$Type <- NULL
#df_Full_Label <- NULL
for(x in df$Names){
  df[which(df$Names == x),"Type"] <- dataraw_feature[which(dataraw_feature$Label == x),"Type"]
  df[which(df$Names == x),"Full_Label"] <- dataraw_feature[which(dataraw_feature$Label == x),"Full_Label"]
}

# Re-load the raw pheno file and subset relevant groups
dataraw_pheno_new = read.csv("file://C:/DATA STORAGE/Projects/PPMI/Data RAW/Baseline_EpigeneticsAdded_140721.csv", header=TRUE, stringsAsFactors=FALSE)


# make classes by defined class	of interest
if(s_Lancet_classes==0){
	dataraw_pheno_new$PDD_Group = NA

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Normal")] <- 0#0
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "MCI")] <- 1#1
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Dementia")] <- 1#2
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
}


if(s_Lancet_classes==1){
	# Load alternative classification based on lancer paper
	lancet_class = data.frame(data.table::fread(Lancetdata, header=TRUE, stringsAsFactors=FALSE)) 

	dataraw_pheno_new$Lancet_Class = lancet_class$BinaryPDD
	dataraw_pheno_new$PDD_Group = dataraw_pheno_new$Lancet_Class

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "NonDementia")] <- 0#0
	
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "Dementia")] <- 1#0
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
	
}



#dataraw_pheno_new[which(dataraw_pheno_new$Conversion_group == "Normal_PDD" |dataraw_pheno_new$Conversion_group == "MCI_PDD" ),"PDD_Group"] <- 1
dataraw_pheno_new <- dataraw_pheno_new[-which(is.na(dataraw_pheno_new$PDD_Group)),]
dataraw_pheno_new$Class <- as.factor(dataraw_pheno_new$PDD_Group)
levels(dataraw_pheno_new$Class) <- c("Stable","Dementia \nConverter ")
dataraw_pheno_new$count <-  1 # Count variable for stacked bar charts



# uses dataraw_pheno_new (data raw) & df (top importance list and such)
#dataraw_pheno_new

dat1 = dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",]
dat2 = dataraw_pheno_new[dataraw_pheno_new$Class=="Dementia \nConverter ",]

# settings plot
s_violin_width = 0.3
s_boxplot_width = s_violin_width/5
s_violin_alpha = 0.3
s_nudge = 0.000


plot_list <- list()

for(x in 1:tophitsamount){
	if(df[x,"Type"] == "CON"){
			  
		# get y ranges
		Yrange = range(c(dataraw_pheno_new[,as.character(df$Names[x])]))
	  

		# make figure
		plot_list[[x]] <- ggplot(data=dat1, aes_string(x=1, y=as.character(df$Names[x]),fill="Class",color="Class")) +
			# right (up) part
			geom_half_violin(side=c("r"),alpha = s_violin_alpha,width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(side=c("r"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "green4",errorbar.length=1)+
			# left (down) part
			geom_half_violin(data=dat2,side=c("l"),alpha = s_violin_alpha, width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(data=dat2,side=c("l"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "red4",errorbar.length=1)+
			# additionals
			ylim(Yrange[1]*0.95, Yrange[2]*1.05)+
			coord_flip()+
			theme_cowplot()+
			theme(legend.position = "none")+
			theme(axis.ticks.y=element_blank())+
			theme(axis.text.y=element_blank())+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
			ylab("")+
			xlab("")+
			scale_fill_manual(values = c("red2","green2"))+
			scale_color_manual(values = c("red3","green3"))# makes the contrast slightly better


	}else{
			
		dataraw_pheno_new[,as.character(df$Names[x])] <- as.factor(dataraw_pheno_new[,as.character(df$Names[x])])

		plot_list[[x]]  <- ggplot(dataraw_pheno_new, aes_string(fill=as.character(df$Names[x]), y="count", x="Class")) + 
			geom_bar(position="fill", stat="identity")+
			coord_flip()+
			ggtitle("")  +
			xlab("")+
			ylab("")+
			theme_cowplot()+
			scale_fill_brewer()+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))
	}
}

for(i in 1:length(plot_list)){
	assign(paste0("plot_",i),value = plot_list[[i]])
}


# illustrate how it looks
#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,
#	  plot_9,plot_10,nrow = 5,ncol = 2)


#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,nrow = 5,ncol = 2)

g_plot = plot_grid(plotlist = plot_list,nrow = ceiling(tophitsamount/2),ncol = 2)

#g_plot = plot_grid(plotlist = plot_list,ncol = 1)

# Save
if(s_Saving){
	png(paste0("Group_differences.png"),width = 4000,height = 600*length(plot_list)/2, res = 350)
		plot(g_plot)
	dev.off()          
}


#-----------------------------------------------------------------------------------------------------#
#							Final version? possibly.
#-----------------------------------------------------------------------------------------------------#

# load data example
#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/Clinical/rf_image.RData")
load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/All/cforest_image.RData")

tophitsmaxamount = 1000 # big number means all
tophitsamount = min(dim(superresult$Combine_data_main$model[[1]]$importance$importance)[1],tophitsmaxamount)
#dataoptions = "Combine_data_main"

# check if varimp already exists else remove
if(exists("temp_Varimp")){rm("temp_Varimp")}

#temp_data = get(dataoptions)
temp_Varimp = superresult$Combine_data_main$model[[1]]$importance$importance

rownames_variimp = rownames(temp_Varimp)

		
ReorderID = NULL
ReorderID = order(temp_Varimp[[1]],decreasing = T) #@RRRnote: [[1]] means class0

if(is.null(ReorderID)){
	temp_Varimp = temp_Varimp[[1]] #@RRRnote: [[1]] means class0
	ReorderID = order(temp_Varimp,decreasing = T)
	temp_Varimp = data.frame(temp_Varimp[ReorderID])
}else{
	temp_Varimp = data.frame(temp_Varimp[ReorderID,1]) # selected col 1 -> means class 0
}




rownames(temp_Varimp) = rownames_variimp[ReorderID]
colnames(temp_Varimp) = "VarImp"



df = data.frame(
  Names=rownames(temp_Varimp)[1:tophitsamount],
  VarImp=temp_Varimp[1:tophitsamount,]
)

# Add additional data labels from the feature labels file
# Add variable information
df$Type <- NULL
#df_Full_Label <- NULL
for(x in df$Names){
  df[which(df$Names == x),"Type"] <- dataraw_feature[which(dataraw_feature$Label == x),"Type"]
  df[which(df$Names == x),"Full_Label"] <- dataraw_feature[which(dataraw_feature$Label == x),"Full_Label"]
}

# Re-load the raw pheno file and subset relevant groups
dataraw_pheno_new = read.csv("file://C:/DATA STORAGE/Projects/PPMI/Data RAW/Baseline_EpigeneticsAdded_140721.csv", header=TRUE, stringsAsFactors=FALSE)


# make classes by defined class	of interest
if(s_Lancet_classes==0){
	dataraw_pheno_new$PDD_Group = NA

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Normal")] <- 0#0
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "MCI")] <- 1#1
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == "Dementia")] <- 1#2
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
}


if(s_Lancet_classes==1){
	# Load alternative classification based on lancer paper
	lancet_class = data.frame(data.table::fread(Lancetdata, header=TRUE, stringsAsFactors=FALSE)) 

	dataraw_pheno_new$Lancet_Class = lancet_class$BinaryPDD
	dataraw_pheno_new$PDD_Group = dataraw_pheno_new$Lancet_Class

	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "NonDementia")] <- 0#0
	
	dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == "Dementia")] <- 1#0
	
	dataraw_pheno_new$PDD_Group = as.numeric(dataraw_pheno_new$PDD_Group)
	
}



#dataraw_pheno_new[which(dataraw_pheno_new$Conversion_group == "Normal_PDD" |dataraw_pheno_new$Conversion_group == "MCI_PDD" ),"PDD_Group"] <- 1
dataraw_pheno_new <- dataraw_pheno_new[-which(is.na(dataraw_pheno_new$PDD_Group)),]
dataraw_pheno_new$Class <- as.factor(dataraw_pheno_new$PDD_Group)
levels(dataraw_pheno_new$Class) <- c("Stable","Dementia \nConverter ")
dataraw_pheno_new$count <-  1 # Count variable for stacked bar charts
# add in class with inverse factor priority; starting with dementia, then normal. This is neede dfor bar plots to hav stable in 1st position; dont know why its being so stubborn!
dataraw_pheno_new$ClassInvPrio = factor(dataraw_pheno_new$Class,levels =c( "Dementia \nConverter ","Stable"))


# uses dataraw_pheno_new (data raw) & df (top importance list and such)
#dataraw_pheno_new

dat1 = dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",]
dat2 = dataraw_pheno_new[dataraw_pheno_new$Class=="Dementia \nConverter ",]

# settings plot
s_violin_width = 0.3
s_boxplot_width = s_violin_width/5
s_violin_alpha = 0.3
s_nudge = 0.000


plot_list <- list()

for(x in 1:tophitsamount){
	if(df[x,"Type"] == "CON"){
			  
		# get y ranges
		Yrange = range(c(dataraw_pheno_new[,as.character(df$Names[x])]))
	  

		# make figure
		plot_list[[x]] <- ggplot(data=dat1, aes_string(x=1, y=as.character(df$Names[x]),fill="Class",color="Class")) +
			# right (up) part
			geom_half_violin(side=c("r"),alpha = s_violin_alpha,width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(side=c("r"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "green4",errorbar.length=1)+
			# left (down) part
			geom_half_violin(data=dat2,side=c("l"),alpha = s_violin_alpha, width = s_violin_width,nudge=s_nudge)+
			geom_half_boxplot(data=dat2,side=c("l"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "red4",errorbar.length=1)+
			# additionals
			ylim(Yrange[1]*0.95, Yrange[2]*1.05)+
			coord_flip()+
			theme_cowplot()+
			theme(legend.position = "none")+
			theme(axis.ticks.y=element_blank())+
			theme(axis.text.y=element_blank())+
			ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
			ylab("")+
			xlab("")+
			scale_fill_manual(values = c("red2","green2"))+
			scale_color_manual(values = c("red3","green3"))# makes the contrast slightly better


	}else{

			
		#start
		dataraw_pheno_new[,as.character(df$Names[x])] <- as.factor(dataraw_pheno_new[,as.character(df$Names[x])])
		
		
		# check if inbalance (num) in colors to be labeled and account for this
		n_cols_stable = length(unique(as.numeric(na.omit(dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",as.character(df$Names[x])]))))
		n_cols_PDD = length(unique(as.numeric(na.omit(dataraw_pheno_new[dataraw_pheno_new$Class!="Stable",as.character(df$Names[x])]))))
		
		# max found colors
		ncolors = max(n_cols_PDD,n_cols_stable)
		
		Reds = RColorBrewer ::brewer.pal(ncolors, "Reds")
		Greens = RColorBrewer ::brewer.pal(ncolors, "Greens")
		Grays = RColorBrewer ::brewer.pal(ncolors, "Greys")
		
		if(ncolors<3){
			Reds = Reds[c(1,2)]
			Greens = Greens[c(1,2)]
			Grays = Grays[c(1,2)]		
		}

		Colorlist = c(Greens[1:n_cols_stable],Reds[1:n_cols_PDD])

		#@RRR omitting NAs would be a possible good idea here!!!
		Raw_plot = ggplot(dataraw_pheno_new, aes_string(x="ClassInvPrio",y="count", fill=paste0("Class:",as.character(df$Names[x])) )) + 
					geom_bar(position="fill",stat="identity")+
					coord_flip()+
					ggtitle("")+
					xlab("")+
					ylab("")+
					theme_cowplot()+
					#scale_fill_manual(values = c("red2","green2","green3"))+
					#scale_fill_brewer(palette = "Reds")+
					ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
					scale_fill_manual(values = Colorlist)+
					theme(legend.position="none")+
					theme(axis.ticks.y=element_blank())+
					theme(axis.text.y=element_blank())
					
					
					
					
		legend_plot = ggplot(dataraw_pheno_new, aes_string(fill=paste0(as.character(df$Names[x])), y="count", x="Class")) + 
					geom_bar(position="fill", stat="identity")+
					coord_flip()+
					ggtitle("")  +
					xlab("")+
					ylab("")+
					theme_cowplot()+
					#scale_fill_manual(values = c("red2","green2","green3"))+
					#scale_fill_brewer(palette = "Reds")+
					ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
					theme(legend.position='right')+
					scale_fill_manual("",values = Grays)
					
					
					
		# Extract legend from the legend_plot
		tmp <- ggplot_gtable(ggplot_build(legend_plot))
		leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
		mylegend <- tmp$grobs[[leg]]


		# add in legend to the right
		plot_list[[x]] = grid.arrange(arrangeGrob(Raw_plot),mylegend,ncol=2,widths=c(9, 1),newpage=TRUE)
		
	}
}

for(i in 1:length(plot_list)){
	assign(paste0("plot_",i),value = plot_list[[i]])
}


# illustrate how it looks
#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,
#	  plot_9,plot_10,nrow = 5,ncol = 2)


#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,nrow = 5,ncol = 2)
n_rows = ceiling(tophitsamount/2)
n_cols = 2
g_plot = plot_grid(plotlist = plot_list,nrow = n_rows,ncol = n_cols)



#g_plot = plot_grid(plotlist = plot_list,ncol = 1)

# Save
Aspect_ratio=c(10,2) #10:2
baseValue=190 #190


if(s_Saving){
	png(paste0("Group_differences.png"),width = baseValue*Aspect_ratio[1]*n_cols,height = baseValue*Aspect_ratio[2]*length(plot_list), res = 350)
		plot(g_plot)
	dev.off()          
}

# if ratio: 			10:2		16:9
# width per image  = 	2000 		2000
# heigth per image  = 	400			1125