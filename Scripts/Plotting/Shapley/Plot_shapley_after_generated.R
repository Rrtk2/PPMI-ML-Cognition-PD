#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#

# General
s_TargerDir = "C:/DATA STORAGE/Projects/PPMI/Results/Images/Shapley 23-11-21/"

# Feature info, Cat/ORD or Cont
FeatureInfo = "C:/DATA STORAGE/Projects/PPMI/Data RAW/FactorLabels_080721.csv"

# Save image
s_Saving = TRUE
s_Aspect_ratio = c(10,10) #10:2 (this looks good)
s_baseValue = 15 #190 (190 per row)

#-----------------------------------------------------------------------------------------------------#
#							Libraries
#-----------------------------------------------------------------------------------------------------#
# Set packages
s_requiredpackages = c("ggplot2","ggfortify","dplyr","cowplot","splitstackshape","tidyverse")

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
#							Functions
#-----------------------------------------------------------------------------------------------------#

minmax = function(x){
	(x-min(x))/(max(x)-min(x))
}

#-----------------------------------------------------------------------------------------------------#
#							Load feature info
#-----------------------------------------------------------------------------------------------------#

# Load Feature info
dataraw_feature = data.frame(data.table::fread(FeatureInfo, header=TRUE, stringsAsFactors=FALSE)) 

#-----------------------------------------------------------------------------------------------------#
#							Load shapley combined
#-----------------------------------------------------------------------------------------------------#

plotname2 = paste0("Weighted_shap.Rdata")
load(paste0(s_TargerDir,plotname2))
#object: weighted_shap

#copy 
weighted_shap_object = weighted_shap

#-----------------------------------------------------------------------------------------------------#
#							Rescale (minmax) the shapley values per model
#-----------------------------------------------------------------------------------------------------#

# rescale
if(FALSE){
	for(i in 3:6){

		temp_data_na = !is.na(weighted_shap_object[,i])
		weighted_shap_object[temp_data_na,i] = round(minmax(weighted_shap_object[temp_data_na,i])*100)
	}
}
# remove combined 
weighted_shap_object = weighted_shap_object[,1:6]
weighted_shap_object = data.frame(weighted_shap_object[,1:2],Full_Label=dataraw_feature[match(weighted_shap_object[,1],dataraw_feature$Label),"Full_Label"],weighted_shap_object[,3:6])

# rename object
Shap_adjusted = weighted_shap_object

#-----------------------------------------------------------------------------------------------------#
#							Saving essesntial stuff
#-----------------------------------------------------------------------------------------------------#
#

if(s_Saving){
	# Text files
	write.table(Shap_adjusted,paste0(s_TargerDir,"Shap_adjusted.txt"),sep="\t",row.names=FALSE,col.names=TRUE)

	# Objects
	plotname2 = paste0("Shap_adjusted.Rdata")
	save(Shap_adjusted, file = paste0(s_TargerDir,plotname2))

}
