#-----------------------------------------------------------------------------------------------------#
#							Functions
#-----------------------------------------------------------------------------------------------------#

# root mean squared error
RMSE = function(predicted, observed){
  sqrt(mean((predicted - observed)^2))
}

# Mode
 Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Machine learning pipeline
MLengine = function(fitControl,s_maxCPUCores,temp_data_train,s_metric,s_MLmethods_select,s_tuneLength,s_kGroups,temp_data_test,s_VERBOSE,subresult,s_MLmethods,s_ROC_Level,s_ROC_direction){
			
	
	#-----------------------------------------------------------------------------------------------------#
	# TRAINING MODEL
	#-----------------------------------------------------------------------------------------------------#
	# Seed for consistancy
	#set.seed(42)

	# create temp result list for each ML method
	temp_resobject = list()

	# save in object
	temp_resobject[["fitControl"]] = fitControl

	# Start Multi-core processing
	# get amount of cores
	temp_max_cores = min(s_maxCPUCores,(detectCores()-1))
	cl <- makePSOCKcluster(max(1,temp_max_cores))
	registerDoParallel(cl)

	# Actual training
	fit <- train(Class ~ ., data = temp_data_train,
	metric=s_metric,
	method = s_MLmethods_select,
	tuneLength = s_tuneLength,
	trControl = fitControl,
	maximize = TRUE)

	#stop Multi-core processing
	stopCluster(cl)

	# Store model in result list
	temp_resobject[["fit"]] = fit

	#-----------------------------------------------------------------------------------------------------#
	# FIND BEST MODEL
	#-----------------------------------------------------------------------------------------------------#
	# OK this is one of the most frustrating issues; match a dynamically changing list(s) to dynamically changing tuning parameters; 
	# approach is check if there are more than 1 paramers (6 base columns + 1 parameter col; thus check if 7), if no: then select the 6th col (this is the tuning col) & compare to resulted list. If yes: identify the amount of added colums (normal these are 7, so 6:end-1. Then check the order of tuning parameters, set the listst in correct order, paste all tuning parameters together and compare.
	if(dim(fit$pred)[2]==(5+s_kGroups)){
		index = fit$pred[,(2+s_kGroups+2)] %in% fit$bestTune
		
	}else{
		
		# Get the tuning results
		RelevantTuneScores = fit$pred#[,6:(dim(fit$pred)[2]-(s_kGroups-1))]
		
		# get the columns of interest
		RelevantTuneScores = RelevantTuneScores[,(match(names(RelevantTuneScores),x=names(fit$bestTune),nomatch = 0))]

		# find out if the tuning has 1 column or multiple
		some_df = c()
		if(is.null(dim(RelevantTuneScores)[1])){
			RelevantTuneScorelength = 1
				# get the combined tuning variables (concatonated)
				for(p in 1:RelevantTuneScorelength){
					some_df = RelevantTuneScores
				}
			}else{
			RelevantTuneScorelength = dim(RelevantTuneScores)[1]
				# get the combined tuning variables (concatonated)
				for(p in 1:RelevantTuneScorelength){
					some_df[p] = paste0(RelevantTuneScores[p,],collapse = "_")
				}
		}
		

		
		# Find which combination is the correct one and identify those
		index = some_df %in% paste0(fit$bestTune,collapse = "_") 
	}

	#-----------------------------------------------------------------------------------------------------#
	# TRAIN
	#-----------------------------------------------------------------------------------------------------#
	# Predict TRAIN set based on best model
	predictions_train <- predict(fit, temp_data_train, na.action = na.omit)
	temp_resobject[["predictions_train"]] = predictions_train

	predictions_train_p <- predict(fit, temp_data_train, type="prob", na.action = na.omit)
	temp_resobject[["predictions_train_p"]] = predictions_train_p


	# Store TRAIN samples & Class
	temp_resobject[["Train_samples"]] = rownames(temp_data_train)
	temp_resobject[["Train_samples_class"]] = data.frame(names=rownames(temp_data_train),Class=temp_data_train$Class)


	#CLASSIFICATION
	resultMCC_TRAIN = RRtest::MCC(data.frame(obs=temp_data_train$Class, pred=predictions_train))
	
	# Store MCC
	temp_resobject[["resultMCC_TRAIN"]] = resultMCC_TRAIN

	# Calculate accuracy
	resultACC_TRAIN = sum(predictions_train==temp_data_train$Class)/length(temp_data_train$Class)

	# Store Accuracy
	temp_resobject[["resultACC_TRAIN"]] = resultACC_TRAIN

	# Calculate ROC for training set, binary or multi
	if(length(levels(fit$pred$obs[index]))==2){

		# binary classification
		AUCval_train = pROC::roc(fit$pred$obs[index], fit$pred[, s_ROC_Level][index],direction=s_ROC_direction) #@RRR levels(fit$pred$pred)[1]
		
		}else{
		
		#Multilevel classification
		AUCval_train = multiclass.roc(fit$pred$obs[index], fit$pred[index,c(3:(3+s_kGroups))])
	}
	#Store Auc of train
	temp_resobject[["AUCval_train"]] = AUCval_train





	#Garbage collect
	gc()


	#-----------------------------------------------------------------------------------------------------#
	# TEST
	#-----------------------------------------------------------------------------------------------------#
	# Predict TEST set based on best model
	predictions_test <- predict(fit, temp_data_test, na.action = na.omit)
	temp_resobject[["predictions_test"]] = predictions_test

	# Store TEST samples
	temp_resobject[["Test_samples"]] = rownames(temp_data_test)
	temp_resobject[["Test_samples_class"]] = data.frame(names=rownames(temp_data_test),Class=temp_data_test$Class)

	# CLASSIFICATION

	# this is a classification only thing; TEST, not train
	predictions_test_p <- predict(fit, temp_data_test, type="prob",na.action = na.omit)
	temp_resobject[["predictions_test_p"]] = predictions_test_p

	# Calculate MCC TEST
	resultMCC_TEST = RRtest::MCC(data.frame(obs=temp_data_test$Class, pred=predictions_test))
	
	# Store MCC
	temp_resobject[["resultMCC_TEST"]] = resultMCC_TEST

	# calculate ACC TEST
	resultACC_test = sum(predictions_test==temp_data_test$Class)/length(temp_data_test$Class)
	temp_resobject[["resultACC_test"]] = resultACC_test

	# Calculate ROC for training set, binary or multi
	if(length(levels(fit$pred$obs[index]))==2){

		# binary classification
		AUCval_test = pROC::roc(temp_data_test$Class, predictions_test_p[, s_ROC_Level],direction=s_ROC_direction)
		
		}else{
		
		#Multilevel classification
		AUCval_test = multiclass.roc(temp_data_test$Class, predictions_test_p)
	}

	# Store ROC of test
	temp_resobject[["AUCval_test"]] = AUCval_test

	# store and show results
	if(s_VERBOSE){
		cat("Results of ML on TRAIN:\n")
		cat("AUC:",AUCval_train$auc[1],"\n")
		#cat(paste0("svmLinear\t",names(resultMCC_TRAIN),"\t",#round(resultMCC_TRAIN,3),"\n"))
		cat("\n")

		cat("Results of ML on TEST:\n")
		cat("AUC:",AUCval_test$auc[1],"\n")
		#cat(paste0("svmLinear\t",names(resultMCC_test),"\t",round(resultMCC_test,3),"\n"))
		cat("\n")
	}




	# get variable importance per model
	temp_resobject[["importance"]] = NA
	if(s_VERBOSE){
		importance <- varImp(fit, scale=TRUE)
		print(importance)
		print(plot(importance))
		temp_resobject[["importance"]] = importance
	}

	# store data in sub-store object
	subresult[[s_MLmethods[i]]] = temp_resobject

	#Garbage collect
	gc()

	# return object
	return(subresult)
}

#-----------------------------------------------------------------------------------------------------#
#							Power estimation of data; define spit ratio!
#							This should be run once; it takes LONG
#-----------------------------------------------------------------------------------------------------#
func_runPowerSplitRatioEstimation = function(dataraw_pheno,data_main,s_ClassID_name,s_maxCPUCores,s_MLmethods,SRoot){
	### --- Data preparation phase --- ###
	
	# Seed
	set.seed(42)

	# Get control group PATNOs
	temp_CTRLsamples = dataraw_pheno$APPRDX == 2 # 1 = PD, 2 = CTRL
	temp_CTRLsamplesnames = dataraw_pheno$PATNO[temp_CTRLsamples]
	temp_TESTsamplesnames = dataraw_pheno$PATNO[!temp_CTRLsamples]
		
	# find the samples which are not control samples (indicated:TEST) and which have a class to be predicted; should result around 130~ samples
		
	# get all test
	temp_data_main = data_main[match(rownames(data_main),x=as.character(temp_TESTsamplesnames)),]

	# get all containg NOT NA class
	temp_data_main = temp_data_main[!is.na(temp_data_main$Class),]

	# dim(temp_data_main)
	# 136 92
	# CORRECT

	# Get relevant cases (in data)
	subdataraw_pheno = dataraw_pheno[match(rownames(temp_data_main),dataraw_pheno$PATNO),]

	a= subdataraw_pheno
	
	# Get gender info from the PHENO, instead of the data itself! (it leads to gen to be included in ALL feature subsets; thats why it should come from pheno!)
	a$gen = dataraw_pheno$gen[match(a$PATNO,dataraw_pheno$PATNO)]
	
	
	### --- Sample selection phase --- ###
	
	# this is the reduced sample set
	Fullsampleset = a$PATNO

	final_res = list()
	s_bootstraps = 100
	initialsplit = 0.8
	for( u in 1:s_bootstraps){ # bootstrap X times (overlap between bootstraps is around 85%@ initialsplit=0.9)
		cat(paste0("Boot",u,"\n"))
		# Apply the stratified sampling to combined set, getting 70% of 50/50 
		sampleset = a$PATNO %in% splitstackshape::stratified(a, c("ClassCol","gen","agebin",s_ClassID_name),size = initialsplit)$PATNO


		temp_train_data = data_main[match(a$PATNO[sampleset],rownames(data_main)),]
		temp_test_data = data_main[match(a$PATNO[!sampleset],rownames(data_main)),]
		
		### --- ML phase --- ###
		# set control
		fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 3, classProb=TRUE,  search = "random", savePredictions = TRUE, summaryFunction = RRtest::MCC) # MCC / twoClassSummary / prSummary
		
		list_res = list()
		max_steps=10
		for(i in 1:max_steps){ # this will select from 1% to 100% of 90% original samples; 10% test
			try({
				# set training / optimisation and control parameters
				temp_df = splitstackshape::stratified(temp_train_data, c("Class"),size = i/max_steps)
				temp_df$Class = as.factor(paste0("Class_",temp_df$Class))
				
				# Start Multi-core processing
				# get amount of cores
				temp_max_cores = min(s_maxCPUCores,(detectCores()-1))
				cl <- makePSOCKcluster(max(1,temp_max_cores))
				registerDoParallel(cl)

				# Actual training
				fit <- train(Class ~ ., data = temp_df,
				metric="MCC",
				method = s_MLmethods,
				tuneLength = 100,
				trControl = fitControl,
				maximize = TRUE)

				#stop Multi-core processing
				stopCluster(cl)
				
				#MCC
				predictions_test <- predict(fit, temp_test_data, na.action = na.omit)
				
				resultMCC_TEST = RRtest::MCC(data.frame(obs=temp_test_data$Class, pred=predictions_test))
				
				# ACC
				acc_tab = table(data.frame(obs=temp_test_data$Class, pred=predictions_test))
				ACC = round((acc_tab[1,1]+acc_tab[2,2])/sum(acc_tab),4)
				
				# AUC
				predictions_test_p <- predict(fit, temp_test_data, type="prob",na.action = na.omit)
				
				AUCval_test = pROC::roc(temp_test_data$Class, predictions_test_p[, s_ROC_Level])
				
				list_res[[i]]=c(round(dim(temp_df)[1]/dim(temp_train_data)[1],4),resultMCC_TEST["MCC"],AUCval_test$auc[1],ACC)
			})
		
		}
		
		res = data.frame(NUM= unlist(lapply(list_res,FUN = function(x){x[1]})), MCC=unlist(lapply(list_res,FUN = function(x){x[2]})), AUC=unlist(lapply(list_res,FUN = function(x){x[3]})),ACC=unlist(lapply(list_res,FUN = function(x){x[4]})))
		
		#plot(x=res$NUM,res$MCC)
		#lines(x=res$NUM,res$AUC)
		
		final_res[[u]] = list(res)
		rm("res")
	}
	
	
	dat = data.frame(lapply(final_res,function(x){x[[1]][,4]}))
	colnames(dat)=1:s_bootstraps
	dat_mean = apply(dat,1,mean)
	dat_median = apply(dat,1,median)
	dat_sd = apply(dat,1,sd)
	dat_num = final_res[[1]][[1]][,1]*initialsplit
	
	df = data.frame(mean=dat_mean,sd=dat_sd,Ratio=dat_num)
	
	ggplot(df, aes(x=Ratio, y=mean)) + 
	geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.02) +
	geom_line() +
	geom_point()
	
	TrainTestRatio = final_res
	
	savename = paste0(SRoot,"/","TrainTestRatio.RData")
	save(TrainTestRatio,file=savename)
	return(TrainTestRatio)

}


#-----------------------------------------------------------------------------------------------------#
#							More info
#-----------------------------------------------------------------------------------------------------#
# Part of the "Machine Learning-based Prediction of Cognitive Outcomes in de novo Parkinson's Disease" project. See https://github.com/Rrtk2/PPMI-ML-Cognition-PD for licencing and more info.