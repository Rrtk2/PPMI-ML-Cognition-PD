# For supplementary
# All metrics (all outcomes)

#-----------------------------------------------------------------------------------------------------#
#							Library
#-----------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#

# Details about ROC curves
	s_ROC_direction = "<"
	s_ROC_Level  = "Class1"

#-----------------------------------------------------------------------------------------------------#
#							1 load + 2 Rename
#-----------------------------------------------------------------------------------------------------#
CM_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results"

# Table has 32 rows; 2*4*4; make counter
CM_table = data.frame(matrix(nrow=4*4*4,ncol=20+7+2))
Colnamess = c("Outcome","Feature subset","Algorithm","NumberOfFeatures","TP","TN","FP","FN","Accuracy","Kappa ","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue","McnemarPValue","Sensitivity","Specificity","Pos Pred Value","Neg Pred Value","Precision","Recall","F1","Prevalence ","Detection Rate","Detection Prevalence","Balanced Accuracy","MCC","AUC","AUC_CI")
colnames(CM_table)=Colnamess


counter = 1

# Class
for( i_class in c(0,1,2,3)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")
			load(paste0(CM_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_model.RData"))
			
			resdata = list(Pred_P = superresult$Combine_data_main$model[[1]]$predictions_test, Pred_P_p = superresult$Combine_data_main$model[[1]]$predictions_test_p,Obs_C = force(superresult$Combine_data_main$model[[1]]$Test_samples_class[,2]),Metrics=superresult$Combine_data_main$model[[1]]$resultMCC_TEST,NumberOfFeatures = length(superresult$Combine_data_main$model[[1]]$fit$coefnames))
			
			minitable = table(data.frame(resdata$Pred_P,resdata$Obs_C))
			
			MLmetrics = caret::confusionMatrix(minitable,positive="Class1")
			
			# reset 
			TP=NA;TN=NA;FP=NA;FN=NA
			
			# assign; but can fail
			TP=minitable["Class0","Class0"]
			TN=minitable["Class1","Class1"]
			FP=minitable["Class1","Class0"]
			FN=minitable["Class0","Class1"]
			
			MCC = as.numeric(resdata$Metrics["MCC"])
			
			rocc = pROC::roc(resdata$Obs_C, resdata$Pred_P_p[,s_ROC_Level], plot=FALSE, legacy.axes=FALSE, percent=TRUE, lwd=2, print.auc=TRUE,direction=s_ROC_direction)
			
			AUC = rocc$auc[1]
			auc_ci = paste0(round(as.numeric(pROC::ci.auc(rocc)),1)[c(1,3)],collapse = " - ",sep="%")
			
			# put in format
			CM_table[counter,] = c(i_class,o_subset,p_algo,resdata$NumberOfFeatures,TP,TN,FP,FN,MLmetrics$overall,MLmetrics$byClass,MCC,round(AUC/100,3),auc_ci)
			
			counter = counter+1
			#browser(auc_ci)
		}

	}


}

#-----------------------------------------------------------------------------------------------------#
#							Cleanup!
#-----------------------------------------------------------------------------------------------------#
# all except starting with ROC_
rm(list = ls()[grep(ls(),pattern = "^CM_",invert = T)])

#-----------------------------------------------------------------------------------------------------#
#							Start cleaning table metrics
#-----------------------------------------------------------------------------------------------------#
CM_table[,-c(1:3,29)] = apply(CM_table[,-c(1:3,29)],2,function(x){try(round(as.numeric(x),3))})

CM_table_MAIN = CM_table[,colnames(CM_table)%in%c("Outcome","Feature subset","Algorithm","NumberOfFeatures","TP","TN","FP","FN","Accuracy","Sensitivity","Specificity","MCC","AUC","AUC_CI")]



#-----------------------------------------------------------------------------------------------------#
#							save
#-----------------------------------------------------------------------------------------------------#
write.table(CM_table,file = paste0(CM_Rootdir,"/Table_metrics_full_SUP_",format(Sys.time(), "%M%S_%d%m%y"),".txt"),sep = "\t",quote = FALSE,row.names=FALSE)
write.table(CM_table_MAIN,file = paste0(CM_Rootdir,"/Table_metrics_main_SUP_",format(Sys.time(), "%M%S_%d%m%y"),".txt"),sep = "\t",quote = FALSE,row.names=FALSE)