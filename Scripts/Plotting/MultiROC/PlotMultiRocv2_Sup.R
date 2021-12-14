

#1) load per class -> Subset -> ML algo the correct object. 
#2) Rename object to current thing
#3) Make 1 plot of all ML methods per Class -> subset; save plot
#4) make and plot grid.

#-----------------------------------------------------------------------------------------------------#
#							Goal
#-----------------------------------------------------------------------------------------------------#
# this image will be the SUPP ROC plot image in the text

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
		
		s_ThickMode = TRUE
		s_LineThickLwd = ifelse(s_ThickMode,2,1) # thick line mode for best or not
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

#  i_class=0;temp_Outcome = "Impairment";o_subset="All";p_algo="rf"

# Class
for( i_class in c(2,3)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	
	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")
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
i_class=0;o_subset="Clinical";
# Class
for( i_class in c(2,3)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	
	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic")){ #c("All","Clinical","Serum","Genetic_Epigenetic")
		
		# Renaming biofluids
		o_subset_rename=o_subset
		if(o_subset=="Serum"){
			o_subset_rename = "Biofluids"
		}

		# Renaming Genetic_Epigenetic
		if(o_subset=="Genetic_Epigenetic"){
			o_subset_rename = "Genetic / Epigenetic"
		}



		# make cforest
		g_cforest = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","cforest"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","cforest"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)
		
		
		# make svmLinear
		g_svmLinear = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","svmLinear"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","svmLinear"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)
		
		
		# make glmnet
		g_glmnet = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","glmnet"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","glmnet"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)
		
		# make rf
		g_rf = pROC::roc(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","rf"))$Obs_C, get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","rf"))$Pred_P[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)
		
		# Check which model is the best based on AUC
		model_best = which(c(g_cforest$auc[1],g_svmLinear$auc[1],g_glmnet$auc[1],g_rf$auc[1])==max(c(g_cforest$auc[1],g_svmLinear$auc[1],g_glmnet$auc[1],g_rf$auc[1])))
		
		# make sure the line widths are in line with found AUC (best)
		LineWidthSeries = 1
		
		LineWidthSeries = c(
		rep(ifelse(1%in%model_best,s_LineThickLwd,1),length(g_cforest$specificities)),
		rep(ifelse(2%in%model_best,s_LineThickLwd,1),length(g_svmLinear$specificities)),
		rep(ifelse(3%in%model_best,s_LineThickLwd,1),length(g_glmnet$specificities)),
		rep(ifelse(4%in%model_best,s_LineThickLwd,1),length(g_rf$specificities)))
		
		
		# Prepare MCCs
		MCC_cforest = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","cforest"))$Metrics["MCC"])
		if(MCC_cforest==-1){MCC_cforest=NA}
		
		MCC_svmLinear = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","svmLinear"))$Metrics["MCC"])
		if(MCC_svmLinear==-1){MCC_svmLinear=NA}
		
		MCC_glmnet = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","glmnet"))$Metrics["MCC"])
		if(MCC_glmnet==-1){MCC_glmnet=NA}
		
		MCC_rf = as.numeric(get(paste0("ROC_",temp_Outcome,"_",o_subset,"_","rf"))$Metrics["MCC"])
		if(MCC_rf==-1){MCC_rf=NA}
		
		# Make total combined
		g_total = pROC::ggroc(list(Cforest = g_cforest, SVMLinear = g_svmLinear, ElasticNet = g_glmnet, RF = g_rf))+
		guides(color = guide_legend("Algorithm",override.aes = list(size = 3) ) )+
		#
		# Title
		#ggtitle(paste0(o_subset_rename," ",temp_Outcome))+
		#
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
		annotate("text", x = s_AUC_x, y = s_Text_top-1*s_Text_step, label = paste0(sprintf("%.2f",round(g_cforest$auc[1]/100,3))),col=gg_color_hue(4)[1], fontface = ifelse(model_best==1,s_BoldThickFont,"plain"),hjust = 1)+
		# svmLinear
		annotate("text", x = s_AUC_x, y = s_Text_top-2*s_Text_step, label = paste0(sprintf("%.2f",round(g_svmLinear$auc[1]/100,3))),col=gg_color_hue(4)[2], fontface =ifelse(model_best==2,s_BoldThickFont,"plain"),hjust = 1)+
		#glmnet
		annotate("text", x = s_AUC_x, y = s_Text_top-3*s_Text_step, label = paste0(sprintf("%.2f",round(g_glmnet$auc[1]/100,3))),col=gg_color_hue(4)[3], fontface = ifelse(model_best==3,s_BoldThickFont,"plain"),hjust = 1)+
		#rf
		annotate("text", x = s_AUC_x, y = s_Text_top-4*s_Text_step, label = paste0(sprintf("%.2f",round(g_rf$auc[1]/100,3))),col=gg_color_hue(4)[4], fontface = ifelse(model_best==4,s_BoldThickFont,"plain"),hjust = 1)+
		#
		# 			MCCs 
		#cforest
		annotate("text", x = s_MCC_x, y = s_Text_top-1*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_cforest,digits = 2))),col=gg_color_hue(4)[1], fontface = ifelse(model_best==1,s_BoldThickFont,"plain"),hjust = 1)+
		# svmLinear
		annotate("text", x = s_MCC_x, y = s_Text_top-2*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_svmLinear,digits = 2))),col=gg_color_hue(4)[2], fontface =ifelse(model_best==2,s_BoldThickFont,"plain"),hjust = 1)+
		#glmnet
		annotate("text", x = s_MCC_x, y = s_Text_top-3*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_glmnet,digits = 2))),col=gg_color_hue(4)[3], fontface = ifelse(model_best==3,s_BoldThickFont,"plain"),hjust = 1)+
		#rf
		annotate("text", x = s_MCC_x, y = s_Text_top-4*s_Text_step, label = paste0(sprintf("%.2f",round(MCC_rf,digits = 2))),col=gg_color_hue(4)[4], fontface = ifelse(model_best==4,s_BoldThickFont,"plain"),hjust = 1)+
		#
		# Theme ; legend
		theme(legend.position = "bottom",
		axis.title.x=element_blank(),
		axis.title.y=element_blank())+
		#guide_legend(title="New Legend Title"))+
		geom_line(lwd=LineWidthSeries)
		
		
		# X  = Specificity
		# Y = Sensivity
		
		
		
		
			
		
		# save plot
		current_plot = g_total
		
		PLOTname = paste0("PLOT_",temp_Outcome,"_",o_subset)
			
		assign(x = PLOTname,value = current_plot)
			

			
	}

}


#-----------------------------------------------------------------------------------------------------#
#							4) make and produce grid with plots
#-----------------------------------------------------------------------------------------------------#

#plotlist=lapply(X = ls()[grep(ls(),pattern = "^PLOT_")],FUN = function(x){get(x)})


# Legend
legend_plot = PLOT_Subjective_complaint_All
tmp <- ggplot_gtable(ggplot_build(legend_plot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
mylegend <- tmp$grobs[[leg]]



# manual way to make sure plot is OK + complex legend workaround
#plotlist = #list(PLOT_Subjective_complaint_All+theme(legend.position="none"),          
#PLOT_Subjective_complaint_Clinical+theme(legend.position="none"),         
#PLOT_Subjective_complaint_Serum+theme(legend.position="none"),
#PLOT_Subjective_complaint_Genetic_Epigenetic+theme(legend.position="none"),         #  
#PLOT_MCIvsPDD_All+theme(legend.position="none"),                
#PLOT_MCIvsPDD_Clinical+theme(legend.position="none"),
#PLOT_MCIvsPDD_Serum+theme(legend.position="none"),
#PLOT_MCIvsPDD_Genetic_Epigenetic+theme(legend.position="none"))

plotlist = list(PLOT_Subjective_complaint_Genetic_Epigenetic+theme(legend.position="none"),          
PLOT_Subjective_complaint_Serum+theme(legend.position="none"),   
PLOT_Subjective_complaint_Clinical+theme(legend.position="none"),
PLOT_Subjective_complaint_All+theme(legend.position="none"),           
PLOT_MCIvsPDD_Genetic_Epigenetic+theme(legend.position="none"),                
PLOT_MCIvsPDD_Serum+theme(legend.position="none"),
PLOT_MCIvsPDD_Clinical+theme(legend.position="none"),
PLOT_MCIvsPDD_All+theme(legend.position="none"))



# Make grid
Gridplot = cowplot::plot_grid(plotlist = plotlist,ncol = 4,nrow = 2)

# make y-label
#y.grob <- textGrob("Sensitivity", 
#                   gp=gpar(fontface="plain", col="black", fontsize=11), rot=90)
## make x-label
#x.grob <- textGrob("Specificity", 
#                   gp=gpar(fontface="plain", col="black", fontsize=11))


# save the image in PDF
pdf(paste0(ROC_Rootdir,"/MultiROC_Sup_",format(Sys.time(), "%M%S_%d%m%y"),".pdf"), width = 16) # Open a new pdf file
	# arrange the plot and add x/y-labels
	grid.arrange(arrangeGrob(Gridplot),mylegend,nrow=2,heights=c(10, 1),vp=viewport(width=0.9, height=0.9))#left = y.grob, bottom = x.grob

	row_x = 0.03
	row_y = 0.96
	
	#add names to Gridplot
	# Sensivity/specificity
	grid.text("Sensitivity (%)", x = row_x+0.015, y = 0.55,rot=90,gp = gpar(fontface = "plain"),just="center")
	grid.text("Specificity (%)", x = 0.51, y = 0.125,rot=0,gp = gpar(fontface = "plain"),just="center")
	
	# Rows
	grid.text("Normal vs. MCI", x = row_x, y = 0.775,rot=90,gp = gpar(fontface = "bold"))
	grid.text("MCI vs. PDD", x = row_x, y = 0.37,rot=90,gp = gpar(fontface = "bold"))
	
	# Cols

	grid.text("Genetic / Epigenetic", 	x = 0.170, y = row_y,rot=0,gp = gpar(fontface = "plain"),just="center")
	grid.text("Biofluids", x = 0.4, y = row_y,rot=0,gp = gpar(fontface = "plain"),just="center")
	grid.text("Clinical", 	x = 0.625, y = row_y,rot=0,gp = gpar(fontface = "plain"),just="center") #clinical
	grid.text("Biological + Clinical", x = 0.85, y = row_y,rot=0,gp = gpar(fontface = "plain"),just="center")#all
	#
	grid.text("Biological markers", x = 0.295, y = 0.99,rot=0,gp = gpar(fontface = "bold"))
	grid.text("Clinical markers", x = 0.625, y = 0.99,rot=0,gp = gpar(fontface = "bold"),just="center")
	grid.text("All markers", x = 0.85, y = 0.99,rot=0,gp = gpar(fontface = "bold"))
dev.off()




