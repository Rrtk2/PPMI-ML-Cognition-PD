#-----------------------------------------------------------------------------------------------------#
#							Note
#-----------------------------------------------------------------------------------------------------#
# For anybody showing the interest it dive deeper into the scripts: Welcome! Thank you for your interest!
# This is the main wrapper, driving the analysis in an iterataive fasion.
# It might be a quite a search if youre investigating a specific question, feel free to contact me (Rick).
#
# This wrapper calls "WorkflowPPMI", a function from "Workflow_predict_Final_MCC_2CLASS_reduced_ZSCORETRAIN.R" using different settings. (Settings)
# The settings that are defined here are the classes of interest, variable subsets and ML algorithms.
# In turn, WorkflowPPMI will work like described in the methods part of the manuscript. (Processing, RFE, evaluation)
# Half-way it calls the "ML_engine.R", the core of the ML used in the study. (Modelling)


#-----------------------------------------------------------------------------------------------------#
#							Libraries
#-----------------------------------------------------------------------------------------------------#
# Set packages
s_requiredpackages = c("devtools","caret","limma","data.table","doParallel","ggplot2","ggfortify","pROC","dplyr","devtools","e1071","cowplot","splitstackshape","party","tidyverse","ggsci","showtext","kernlab","glmnet","randomForest","ggpmisc")

# Installer & loader
if(FALSE){
	for (i in s_requiredpackages) {
		if (!requireNamespace(i, quietly = TRUE))
			BiocManager::install(as.character(i))  # dependencies = c("Depends", "Imports")
		library(as.character(i), character.only = TRUE)
		#print(i)
	}
}

# Loader
for (i in s_requiredpackages) {
	library(as.character(i), character.only = TRUE)
	#print(i)
}


#-----------------------------------------------------------------------------------------------------#
#							INPUT DATA (Conditional settings based on user)
#-----------------------------------------------------------------------------------------------------#
# Feature info, Cat/ORD or Cont
FeatureInfo = "C:/DATA STORAGE/Projects/PPMI/Data RAW/FactorLabels_080721.csv"

# Pheno / Data
BaselineClin_new = "C:/DATA STORAGE/Projects/PPMI/Data RAW/Baseline_SCD_201021.csv"

# Functions
source("C:/Users/p70072451/Documents/GitHub/PPMI-ML-Cognition-PD/Scripts/Machine learning/FuctionList.R")

# Lancet paper styled classification 
Lancetdata = "C:/DATA STORAGE/Projects/PPMI/Data RAW/BinaryPDDlist.csv"

source("C:/Users/p70072451/Documents/GitHub/PPMI-ML-Cognition-PD/Scripts/Machine learning/Workflow_predict_Final_MCC_2CLASS_reduced_ZSCORETRAIN.R")


#-----------------------------------------------------------------------------------------------------#
#							Run main algorithm
#-----------------------------------------------------------------------------------------------------#
# Class
# Gotten upgrade:
# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD, 3 = Subjective Complaint, 999 = OLD Impairment
for( i_class in c(0,1,2,3)){ 

	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic","ALLnonClin")){ #c("All","Clinical","Serum","Genetic_Epigenetic") #ALLnonClin # "TEST" (contains clinical cognitive markers)

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")

			WorkflowPPMI(s_MLmethods=p_algo,s_Outcome_type=i_class,s_feature_subset=o_subset,FeatureInfo,BaselineClin_new,Lancetdata,SRoot="C:/DATA STORAGE/Projects/PPMI/Results",s_metric="MCC")

		}

	}


}

#-----------------------------------------------------------------------------------------------------#
#							Clinical-COG and COG; sensitivity test
#-----------------------------------------------------------------------------------------------------#
# Class
# Gotten upgrade:
# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD, 3 = Subjective Complaint, 999 = OLD Impairment
for( i_class in c(0)){ 

	# Subset
	for( o_subset in c("COG","Clinical_COG")){ #c("All","Clinical","Serum","Genetic_Epigenetic") #ALLnonClin # "TEST" (contains clinical cognitive markers)

		# Machine learning algorithm
		for( p_algo in c("cforest")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")

			WorkflowPPMI(s_MLmethods=p_algo,s_Outcome_type=i_class,s_feature_subset=o_subset,FeatureInfo,BaselineClin_new,Lancetdata,SRoot="C:/DATA STORAGE/Projects/PPMI/Results",s_metric="MCC")

		}

	}


}

#-----------------------------------------------------------------------------------------------------#
#							More info
#-----------------------------------------------------------------------------------------------------#
# Part of the "Machine Learning-based Prediction of Cognitive Outcomes in de novo Parkinson's Disease" project. See https://github.com/Rrtk2/PPMI-ML-Cognition-PD for licencing and more info.