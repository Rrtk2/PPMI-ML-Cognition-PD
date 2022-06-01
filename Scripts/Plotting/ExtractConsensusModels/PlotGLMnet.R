#-----------------------------------------------------------------------------------------------------#
#							Plotting of Ctree 
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
	for( o_subset in c("All")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("glmnet")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "cforest")
			#browser()
			# Show progress
			print(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo))
			# New load (layzLoad), faster, less RAM; convert .RData -> .rdb/.rdx
			#e = local({load(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_model.RData")); environment()})
			#tools:::makeLazyLoadDB(e, "New")
			#lazyLoad("New")
			
			# old style
			load(paste0(ROC_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_model.RData"))
			
			# remove all
			#rm(list = ls()[grep(ls(),pattern = "^ROC_",invert = T)])
			gc()
		}

	}


}

  
pdf(paste0(ROC_Rootdir,"/SurroTree_SUP_",format(Sys.time(), "%M%S_%d%m%y"),".pdf"), width = 7, height = 5) # Open a new pdf file
	
y <- superresult$Combine_data_main$model[[1]]$fit$finalModel
s_lamb = as.numeric(superresult$Combine_data_main$model[[1]]$fit$bestTune[2])
s_alph = as.numeric(superresult$Combine_data_main$model[[1]]$fit$bestTune[1])


dev.off()
