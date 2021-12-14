# very good link: http://uc-r.github.io/iml-pkg

#Example Shapley
#https://cran.r-project.org/web/packages/iml/vignettes/intro.html
# check out https://rpubs.com/subasish/577019
# check out https://ema.drwhy.ai/shapley.html
#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#

# general settings
DV_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results" # results dir

# model settings
s_ClassIndex = 1 # 1 = Normal; 2 = Diseased
s_Sample_size_MCS = 10000 # 10K Monte-Carlo-Simulations (reps) set to roughly (p-2)!

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
#							Select the 
#-----------------------------------------------------------------------------------------------------#


#i_class=0;temp_Outcome = ifelse(i_class,"LancetClass","OwnClass");o_subset="Clinical";p_algo="rf" # OWN CLINICAL RF


#i_class=0;temp_Outcome = ifelse(i_class,"LancetClass","OwnClass");o_subset="All";p_algo="cforest" # OWN ALL CFOREST

#i_class=0;temp_Outcome = ifelse(i_class,"LancetClass","OwnClass");o_subset="All";p_algo="rf" # OWN ALL RF

#i_class=0;temp_Outcome = ifelse(i_class,"LancetClass","OwnClass");o_subset="All";p_algo="glmnet" # OWN ALL GLMNET

i_class=0;temp_Outcome = "Impairment";o_subset="Serum";p_algo="rf" # OWN ALL SVMLINEAR

# Class
# Class
for( i_class in c(0)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	
	# Subset
	for( o_subset in c("All")){ #,"Clinical","Serum","Genetic_Epigenetic"  #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")
		
			set.seed(42)		
			#-----------------------------------------------------------------------------------------------------#
			#							Load data
			#-----------------------------------------------------------------------------------------------------#
			#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/Clinical/rf_image.RData")
			load(paste0(DV_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_image.RData"))



			#-----------------------------------------------------------------------------------------------------#
			#							Load objects in format
			#-----------------------------------------------------------------------------------------------------#
			# Model
			MLmodel = superresult$Combine_data_main$model[[1]]$fit#$finalModel
			
			# Training data
			SelectedData = temp_data_test # EVALUATION EQUEALS TEST

			# set correct type 
			s_type = "prob" #prob
			#-----------------------------------------------------------------------------------------------------#
			#							Custom prediction function
			#-----------------------------------------------------------------------------------------------------#

			# 1. create a data frame with just the features (PER MODEL)
			features <- as.data.frame(SelectedData)[,MLmodel$coefnames]

			# 2. Create a vector with the actual responses
			#response <- as.numeric(predict(MLmodel, (features),type = "p")[,1]) #NORMAL

			response <- as.numeric(predict(MLmodel, (features),type = s_type)[,s_ClassIndex]) #NORMAL




			# 3. Create custom predict function that returns the predicted values as a
			#    vector (probability of purchasing in our example)
			pred <- function(model, newdata)  {
			  results <- as.data.frame(predict(model, (newdata),type = s_type))
			  return(results[[s_ClassIndex]]) # TO BE CLASSFIED AS CLASS0 (NORMAL)
			}

			# example of prediction output
			#pred(MLmodel, features) %>% head()
			# works!

			#-----------------------------------------------------------------------------------------------------#
			#							Formatting part
			#-----------------------------------------------------------------------------------------------------#

			predictor_MLmodel <- Predictor$new(
			  model = MLmodel, 
			  data = features, 
			  y = as.numeric(response), 
			  predict.function = pred
			  )


			#str(predictor_MLmodel)


			# Feature Importance
			## Shifting each future, and measring how much the performance drops ## 
			#imp_MLmodel = FeatureImp$new(predictor_MLmodel, loss = "logLoss") #ce=classification error
			#plot(imp_MLmodel)
			#p1 = plot(imp_MLmodel) + ggtitle("RF")
			#gridExtra::grid.arrange(p1, nrow = 1)


			#-----------------------------------------------------------------------------------------------------#
			#							--- DISABLED ---
			#							Shapley LOCAL
			#-----------------------------------------------------------------------------------------------------#

			if(FALSE){
				# identify obs with highest and lowest probabilities
				high <- as.numeric(predict(MLmodel, (features),type = s_type)[,s_ClassIndex]) %>% as.vector() %>% which.max()

				low <- as.numeric(predict(MLmodel, (features),type = s_type)[,s_ClassIndex]) %>% as.vector() %>% which.min()

				high_prob_ob <- features[high, ] #select sample that is predicted the most confident  (THUS: CLASS 0 (NORMAL))

				low_prob_ob <- features[low, ] #select sample that is predicted the least confident (THUS: CLASS 1 (DISEASED))

				shapley_rf_high <- Shapley$new(predictor_MLmodel,x.interest = high_prob_ob,sample.size = s_Sample_size_MCS) #sample.size = The number of Monte Carlo samples for estimating the Shapley value.
				plot(shapley_rf_high)

				shapley_rf_low <- Shapley$new(predictor_MLmodel,x.interest = low_prob_ob,sample.size = s_Sample_size_MCS)
				plot(shapley_rf_low)

				# fit local model
				ps1 = plot(shapley_rf_high) + ggtitle("Top contributing to normal")
				ps2 = plot(shapley_rf_low) + ggtitle("Top contributing to diseased")
				gridExtra::grid.arrange(ps1, ps2, nrow = 1)


				shapley_rf_ALL <- Shapley$new(predictor_MLmodel,x.interest = features,sample.size = s_Sample_size_MCS)
				plot(shapley_rf_ALL)
				ps3 = plot(shapley_rf_ALL) + ggtitle("ALL")


				gridExtra::grid.arrange(ps1, ps2, ps3, nrow = 1)
			}
			#-----------------------------------------------------------------------------------------------------#
			#							Shaploop
			#-----------------------------------------------------------------------------------------------------#
			library("future")
			library("future.callr")
			# Creates a PSOCK cluster with 2 cores
			# Loop through the samples, explaining one instance at a time.
			shap_values <- vector("list", nrow(features))  # initialize the results list.
			
			system.time({
			  for (i in seq_along(shap_values)) {
				set.seed(224)
				shap_values[[i]] = iml::Shapley$new(predictor_MLmodel, x.interest = features[i, ],
													 sample.size = s_Sample_size_MCS)$results
				shap_values[[i]]$sample_num <- i  # identifier to track our instances.
			  }
			  data_shap_values <- dplyr::bind_rows(shap_values)  # collapse the list.
			})




			#-----------------------------------------------------------------------------------------------------#
			#							--- DISABLED ---
			#								Plots
			#-----------------------------------------------------------------------------------------------------#
			if(FALSE){		
				#> colnames(data_shap_values)
				#[1] "feature"       "phi"           "phi.var"      
				#[4] "feature.value" "sample_num" 
				boxplot(phi~feature,data=data_shap_values,las=2,horizontal=TRUE)
				# colored by probability
				# scatterplot thing

				# MAGNITUDE PLOT
				ggplot(data_shap_values,aes(factor(feature),abs(phi)))+geom_boxplot()+
				coord_flip()
				#geom_point(aes(),position=position_dodge(width=0.5))+
				  
				# DIRECTION PLOT
				ggplot(data_shap_values,aes(factor(feature),(phi)))+
				geom_point(aes(color=response[data_shap_values$sample_num]),position=position_dodge(width=0.5)) +scale_color_continuous()+coord_flip()+geom_hline(yintercept=0)

				# global importance
				#boxplot(abs(phi)~feature,data=data_shap_values,las=2,horizontal=TRUE)
			}
			#-----------------------------------------------------------------------------------------------------#
			#							Summary for combined sample
			#-----------------------------------------------------------------------------------------------------#

			Unique_feature_List = unique(data_shap_values$feature)
			FinalRes = data.frame(feature=Unique_feature_List,phi=NA,phivar=NA)
			for( i in 1:length(Unique_feature_List)){
				Current_Feature = Unique_feature_List[i]
				Current_DataIndex = data_shap_values$feature==Current_Feature
				Current_Data = data_shap_values[Current_DataIndex,]

				Phi_avg = mean(abs(Current_Data$phi))
				PhiVar_avg = mean(abs(Current_Data$phi.var))
			 
				FinalRes[i,] = list(feature=Current_Feature,phi=Phi_avg,phivar=PhiVar_avg)
			}


			#-----------------------------------------------------------------------------------------------------#
			#							Show using summary values
			#-----------------------------------------------------------------------------------------------------#


			FinalRes <- transform(FinalRes, 
									  Feature = reorder(feature, abs(phi)))
									  
			g_plot = ggplot(FinalRes, aes(x=Feature, y=abs(phi))) + geom_bar(stat="identity") + coord_flip() + scale_y_continuous('') + scale_x_discrete('')+ ggtitle(paste0("Global Shapley contribution, ",temp_Outcome,"_",o_subset,"_",p_algo))

			#-----------------------------------------------------------------------------------------------------#
			#							Saving essesntial stuff
			#-----------------------------------------------------------------------------------------------------#
			#
			
			if(s_Saving){
				plotname = paste0(temp_Outcome,"_",o_subset,"_",p_algo,"_ShapleyGlobal.png")
				png(paste0(s_TargerDir,plotname),width = s_baseValue*s_Aspect_ratio[1]*17,height = s_baseValue*s_Aspect_ratio[2]*length(FinalRes$feature), res = 350)
					plot(g_plot)
				dev.off()          
			
				# Objects
				plotname2 = paste0(temp_Outcome,"_",o_subset,"_",p_algo,"_ShapleyGlobal.Rdata")
				save(FinalRes, data_shap_values, g_plot, file = paste0(s_TargerDir,plotname2))
			
			}
			
		}
		
	}
	
}