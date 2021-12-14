#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#

# general settings
DV_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results/Images/Shapley 23-11-21" # results shapley dir

# Save image
s_Saving = TRUE
s_Aspect_ratio = c(10,10) #10:2 (this looks good)
s_baseValue = 15 #190 (190 per row)
s_TargerDir = "C:/DATA STORAGE/Projects/PPMI/Results/Images/Shapley 23-11-21/"


#-----------------------------------------------------------------------------------------------------#
#							Libraries
#-----------------------------------------------------------------------------------------------------#
# Set packages
s_requiredpackages = c("devtools","caret","limma","data.table","doParallel","ggplot2","ggfortify","pROC","dplyr","devtools","e1071","cowplot","splitstackshape","party","tidyverse","ggsci","showtext","kernlab","glmnet","randomForest","ggpmisc","iml")

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
#							Load shapley value objects
#-----------------------------------------------------------------------------------------------------#


#Impairment_All_cforest
load(paste0(DV_Rootdir,"/Impairment_All_cforest_ShapleyGlobal.Rdata"))
Impairment_All_cforest = FinalRes

#Impairment_All_glmnet
load(paste0(DV_Rootdir,"/Impairment_All_glmnet_ShapleyGlobal.Rdata"))
Impairment_All_glmnet = FinalRes

#Impairment_All_rf
load(paste0(DV_Rootdir,"/Impairment_All_rf_ShapleyGlobal.Rdata"))
Impairment_All_rf = FinalRes

#Impairment_All_svmLinear
load(paste0(DV_Rootdir,"/Impairment_All_svmLinear_ShapleyGlobal.Rdata"))
Impairment_All_svmLinear = FinalRes

#Impairment_Clinical_rf
#load(paste0(DV_Rootdir,"/Impairment_Clinical_rf_ShapleyGlobal.Rdata"))
#Impairment_Clinical_rf = FinalRes


#-----------------------------------------------------------------------------------------------------#
#							Check occurrence in ALL models
#-----------------------------------------------------------------------------------------------------#
Occurrence_list = list(
cforest = Impairment_All_cforest$feature,
glmnet = Impairment_All_glmnet$feature,
rf = Impairment_All_rf$feature,
svmLinear = Impairment_All_svmLinear$feature)

Occurrence_table = table(unlist(Occurrence_list))


Occurrence_table = Occurrence_table[order(Occurrence_table,decreasing = T)]
head(Occurrence_table)

#-----------------------------------------------------------------------------------------------------#
#							Make weighted shapley in ALL models
#-----------------------------------------------------------------------------------------------------#
weighted_shap=data.frame(Feature=names(Occurrence_table),occurrence=0,cforest=0,glmnet=0,rf=0,svmLinear=0,Combined=0)

# Add in the occurrence
weighted_shap$occurrence = as.numeric(Occurrence_table)

weighted_shap[,"cforest"] = as.numeric(attr(Impairment_All_cforest$Feature,"scores"))[match(weighted_shap$Feature,names(attr(Impairment_All_cforest$Feature,"scores")))]

weighted_shap[,"glmnet"] = as.numeric(attr(Impairment_All_glmnet$Feature,"scores"))[match(weighted_shap$Feature,names(attr(Impairment_All_glmnet$Feature,"scores")))]

weighted_shap[,"rf"] = as.numeric(attr(Impairment_All_rf$Feature,"scores"))[match(weighted_shap$Feature,names(attr(Impairment_All_rf$Feature,"scores")))]

weighted_shap[,"svmLinear"] = as.numeric(attr(Impairment_All_svmLinear$Feature,"scores"))[match(weighted_shap$Feature,names(attr(Impairment_All_svmLinear$Feature,"scores")))]

# Rough indication on how scores relate
boxplot(weighted_shap[,3:6])

# normalization of all values (to be added as equal AND positive)
if(FALSE){
	boxplot(scale(weighted_shap[,3:6])-min(scale(weighted_shap[,3:6]),na.rm = T))
	weighted_shap[,3:6] = scale(weighted_shap[,3:6])-min(scale(weighted_shap[,3:6]),na.rm = T)
}
# this removes mean effect, which is higher in better performers
# should not normalize

# Replace NAs by 0, combined score needs to evalueate all the models; so using 2 out of 4 models in some cases is wrong
#weighted_shap[is.na(weighted_shap)] = 0

# combine resutls (additive, features scoring more often will result higer)
weighted_shap$Combined = apply(weighted_shap[,3:6],1,function(x){mean(na.omit(x))})
plot(weighted_shap$Combined)

# combine resutls (additive, features scoring more often will result higer
weighted_shap$Rank = rank(-weighted_shap$Combined,ties.method = "min")

#-----------------------------------------------------------------------------------------------------#
#							Saving essesntial stuff
#-----------------------------------------------------------------------------------------------------#
#

if(s_Saving){
	# Text files
	write.table(weighted_shap,paste0(s_TargerDir,"Weighted_shap.txt"),sep="\t",row.names=FALSE,col.names=TRUE)

	# Objects
	plotname2 = paste0("Weighted_shap.Rdata")
	save(weighted_shap, file = paste0(s_TargerDir,plotname2))

}