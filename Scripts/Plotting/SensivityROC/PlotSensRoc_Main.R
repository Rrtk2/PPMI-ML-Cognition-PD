

#1) load per class -> Subset -> ML algo the correct object. 
#2) Rename object to current thing
#3) Make 1 plot of all ML methods per Class -> subset; save plot
#4) make and plot grid.

#-----------------------------------------------------------------------------------------------------#
#							Goal
#-----------------------------------------------------------------------------------------------------#
# this image will be the MAIN ROC plot image in the text

#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#
# Details about ROC curves
	s_ROC_direction = "<"
	s_ROC_Level  = "Class1"
	
	# make settings for AUC/MCC locations
		s_AUC_x = 20#25
		s_MCC_x = 0
		s_Text_top = 30
		s_Text_step = 6
		
		s_ThickMode = FALSE
		s_defaultLwd = 1.5
		s_LineThickLwd = ifelse(s_ThickMode,s_defaultLwd+1,s_defaultLwd) # thick line mode for best or not
		s_BoldThickFont = ifelse(s_ThickMode,"bold","plain")
		
#-----------------------------------------------------------------------------------------------------#
#							Library
#-----------------------------------------------------------------------------------------------------#
library(pROC)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

#-----------------------------------------------------------------------------------------------------#
#							1 load + 2 Rename
#-----------------------------------------------------------------------------------------------------#
ROC_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results"

#  i_class=0;temp_Outcome = "Impairment";o_subset="All";p_algo="cforest"

# Class
for( i_class in c(0)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	
	# Subset
	for( o_subset in c("Clinical","Clinical_COG","COG")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "cforest")
			#browser()
			# Show progress
			print(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo))
			# New load (layzLoad), faster, less RAM; convert .RData -> .rdb/.rdx
			#e = local({load(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_model.RData")); environment()})
			#tools:::makeLazyLoadDB(e, "New")
			#lazyLoad("New")
			
			# old style
			load(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_model.RData"))
			
			# make name
			ROCname = paste0("ROC_",temp_Outcome,"_",o_subset,"_",p_algo)
			
			# get data
			resdata = list(Pred_P = force(superresult$Combine_data_main$model[[1]]$predictions_test_p), Obs_C = force(superresult$Combine_data_main$model[[1]]$Test_samples_class[,2]),Metrics=force(superresult$Combine_data_main$model[[1]]$resultMCC_TEST))
			
			# assign data
			assign(x = ROCname,value = resdata)
			
			# remove all
			#rm(list = ls()[grep(ls(),pattern = "^ROC_",invert = T)])
			gc()
		}

	}


}

#-----------------------------------------------------------------------------------------------------#
#							Cleanup!
#-----------------------------------------------------------------------------------------------------#
# all except starting with ROC_
rm(list = ls()[grep(ls(),pattern = "^ROC_|^s_",invert = T)])

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#-----------------------------------------------------------------------------------------------------#
#							3 Make plot per class:subset
#-----------------------------------------------------------------------------------------------------#

# force gray scale between quite white to black
#colfunc <- colorRampPalette(c("#000000","#000000")) # greys D3D3D3
#colfunc <- colorRampPalette(c("#85442B","#F79B67")) # copper
colfunc = gg_color_hue

i_class=0
# Class
# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
if(i_class==0){temp_Outcome = "Impairment"}
if(i_class==1){temp_Outcome = "PDD"}
if(i_class==2){temp_Outcome = "MCIvsPDD"}
if(i_class==3){temp_Outcome = "Subjective_complaint"}


# Subset
#c("All","Clinical","Serum","Genetic_Epigenetic")
o_subset1 = "Clinical"
o_subset2 = "Clinical_COG"
o_subset3 = "COG"

## Renaming biofluids
#o_subset_rename=o_subset
#if(o_subset=="Serum"){
#	o_subset_rename = "Biofluids"
#}
#
#		# Renaming Genetic_Epigenetic
#		if(o_subset=="Genetic_Epigenetic"){
#			o_subset_rename = "Genetic / Epigenetic"
#		}



# make Clinical
g_Clinical = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset1,"_","cforest"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset1,"_","cforest"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction,color="white")


# make Clinical_COG
g_Clinical_COG = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset2,"_","cforest"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset2,"_","cforest"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)


# make COG
g_COG = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset3,"_","cforest"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset3,"_","cforest"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction,colour="red")


# Check which model is the best based on AUC
model_best = which(c(g_Clinical$auc[1],g_Clinical_COG$auc[1],g_COG$auc[1])==max(c(g_Clinical$auc[1],g_Clinical_COG$auc[1],g_COG$auc[1])))





# Prepare MCCs
MCC_cforest = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset1,"_","cforest"))$Metrics["MCC"])
if(MCC_cforest==-1){MCC_cforest=NA}

MCC_svmLinear = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset2,"_","cforest"))$Metrics["MCC"])
if(MCC_svmLinear==-1){MCC_svmLinear=NA}

MCC_glmnet = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset3,"_","cforest"))$Metrics["MCC"])
if(MCC_glmnet==-1){MCC_glmnet=NA}




# Black n white
#s_colorset = c("grey20","grey45","grey70")

# cforest (red) color
interval_col = 10 #(10 = to dark. -10 = to light.)
s_colorset = hcl(c(15,15,15),l=c(65,65-interval_col,65-(interval_col*2)),c=100)

s_size = 1.5

# full table
g_total = pROC::ggroc(list(Clinical = g_Clinical, COG = g_COG, Clinical_COG = g_Clinical_COG), size = s_size,aes = c("colour","size"),legacy.axes = FALSE)+ # in aes "linetype"
scale_color_manual(name = "Feature set: ",values=c("Clinical" = s_colorset[1], "COG" = s_colorset[2],"Clinical_COG" = s_colorset[3]),labels = c("Clinical","Cognitive only","Clinical minus cognitive"))+
#scale_linetype_manual(name = "Feature set: ", values=c("Clinical" = 1, "COG" = 2,"Clinical_COG" = 3),labels = c("Clinical","Cognitive only","Clinical minus cognitive"))+
guides(color = guide_legend(override.aes = list(size = 1)))+
# ABline
geom_abline(slope = 1,intercept = 100,linetype = "dashed",color="gray60")+
#
# 			Legend for AUC/MCC "annotation"
#AUC
annotate("text", x = s_AUC_x, y = s_Text_top, label = paste0("AUC"),col="black", fontface ="plain",hjust = 1)+
#MCC
annotate("text", x = s_MCC_x, y = s_Text_top, label = paste0("MCC"),col="black", fontface ="plain",hjust = 1)+
#
# 			AUCs 
#cforest
annotate("text", x = s_AUC_x, y = s_Text_top-1*s_Text_step, label = paste0(sprintf("%.2f",round(g_Clinical$auc[1]/100,3))),col=s_colorset[1], fontface = ifelse(model_best==1,s_BoldThickFont,"plain"),hjust = 1)+
# svmLinear
annotate("text", x = s_AUC_x, y = s_Text_top-3*s_Text_step, label = paste0(sprintf("%.2f",round(g_Clinical_COG$auc[1]/100,3))),col=s_colorset[3], fontface =ifelse(model_best==3,s_BoldThickFont,"plain"),hjust = 1)+
#glmnet
annotate("text", x = s_AUC_x, y = s_Text_top-2*s_Text_step, label = paste0(sprintf("%.2f",round(g_COG$auc[1]/100,3))),col=s_colorset[2], fontface = ifelse(model_best==2,s_BoldThickFont,"plain"),hjust = 1)+
# 			MCCs 
#cforest
annotate("text", x = s_MCC_x, y = s_Text_top-1*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_cforest,digits = 2))),col=s_colorset[1], fontface = ifelse(model_best==1,s_BoldThickFont,"plain"),hjust = 1)+
# svmLinear
annotate("text", x = s_MCC_x, y = s_Text_top-3*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_svmLinear,digits = 2))),col=s_colorset[3], fontface =ifelse(model_best==3,s_BoldThickFont,"plain"),hjust = 1)+
#glmnet
annotate("text", x = s_MCC_x, y = s_Text_top-2*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_glmnet,digits = 2))),col=s_colorset[2], fontface = ifelse(model_best==2,s_BoldThickFont,"plain"),hjust = 1)+
# Theme ; legend
theme(legend.position = "bottom",
axis.title.x=element_blank(),
axis.title.y=element_blank(),
panel.border = element_blank(), 
#panel.grid.major = element_blank(),
#panel.grid.minor = element_blank(),
panel.background = element_rect(fill = "grey96"))

#guide_legend(title="New Legend Title"))+
#geom_line(lwd=LineWidthSeries,linetype=LineTypeSeries,alpha=1)+
g_total

# X  = Specificity
# Y = Sensivity




	

# save plot
current_plot = g_total

PLOTname = paste0("PLOT_",temp_Outcome)
	
assign(x = PLOTname,value = current_plot)
	

	





#-----------------------------------------------------------------------------------------------------#
#							4) make and produce grid with plots
#-----------------------------------------------------------------------------------------------------#

#plotlist=lapply(X = ls()[grep(ls(),pattern = "^PLOT_")],FUN = function(x){get(x)})


# Legend
legend_plot = PLOT_Impairment
tmp <- ggplot_gtable(ggplot_build(legend_plot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
mylegend <- tmp$grobs[[leg]]



# manual way to make sure plot is OK + complex legend workaround
#plotlist = #list(PLOT_PDD_All+theme(legend.position="none"),          
#PLOT_PDD_Clinical+theme(legend.position="none"),         
#PLOT_PDD_Serum+theme(legend.position="none"),
#PLOT_PDD_Genetic_Epigenetic+theme(legend.position="none"),         #  
#PLOT_Impairment_All+theme(legend.position="none"),                
#PLOT_Impairment_Clinical+theme(legend.position="none"),
#PLOT_Impairment_Serum+theme(legend.position="none"),
#PLOT_Impairment_Genetic_Epigenetic+theme(legend.position="none"))

plotlist = list(PLOT_Impairment+theme(legend.position="none"))



# Make grid
Gridplot = cowplot::plot_grid(plotlist = plotlist,ncol = 1,nrow = 1)

# make y-label
#y.grob <- textGrob("Sensitivity", 
#                   gp=gpar(fontface="plain", col="black", fontsize=11), rot=90)
## make x-label
#x.grob <- textGrob("Specificity", 
#                   gp=gpar(fontface="plain", col="black", fontsize=11))


# save the image in PDF
pdf(paste0(ROC_Rootdir,"/SensROC_Main_",format(Sys.time(), "%M%S_%d%m%y"),".pdf"), width = 5, height = 5) # Open a new pdf file
	# arrange the plot and add x/y-labels
	grid.arrange(arrangeGrob(Gridplot),mylegend,nrow=2,heights=c(10, 1),vp=viewport(width=0.9, height=0.9))#left = y.grob, bottom = x.grob

	row_x = 0.03
	row_y = 0.96
	
	#add names to Gridplot
	# Sensivity/specificity
	grid.text("Sensitivity (%)", x = row_x+0.015, y = 0.55,rot=90,gp = gpar(fontface = "plain"),just="center")
	grid.text("Specificity (%)", x = 0.51, y = 0.125,rot=0,gp = gpar(fontface = "plain"),just="center")
	
	# Rows
	#grid.text("Dementia Outcome", x = row_x, y = 0.775,rot=90,gp = gpar(fontface = "bold"))
	#grid.text("Cognitive Impairment Outcome", x = row_x, y = 0.37,rot=90,gp = gpar(fontface = "bold"))
	
	# Cols

	grid.text("Cognitive feature sensitivity", 	x = 0.5, y = row_y,rot=0,gp = gpar(fontface = "bold"),just="center")
	
dev.off()




