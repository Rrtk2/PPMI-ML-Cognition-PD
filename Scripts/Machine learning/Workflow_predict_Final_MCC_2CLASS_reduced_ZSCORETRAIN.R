WorkflowPPMI = function(s_MLmethods,s_Outcome_type,s_feature_subset,FeatureInfo,BaselineClin_new,Lancetdata,SRoot=NA,s_metric="MCC"){
	
	# classes update:
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD

	#-----------------------------------------------------------------------------------------------------#
	#							Settings
	#-----------------------------------------------------------------------------------------------------#
	# General settings
	s_ZscoreNorm		= TRUE 				# Z-score normalisation based on control samples, then test is normalized using control specs; if no CTRL samples found, do normal Z-score normalisation
	#s_FilterInvasive 	= FALSE				# Filters the PRS, CSF and individual genetypes
	s_Saving			= TRUE				# Saves objects, images 

	s_VERBOSE 			= TRUE
	s_Impute 			= TRUE				# T/F

	# Groups
	s_ClassID_name 		= "Longitudinal_diag" 		# Keep Longitudinal_diag
	s_UseStratifiedTest = FALSE				# Stratified test set 1:1
	
	# ML
	#s_metric 			= "MCC"			# TPR, FPR, FNR, TNR, PPV, FDR, FOR, NPV, PLR, NLR, MK, DOR, BA, F1, FMI, MCC, TS, Prevalence, Prevalence_THR, ACC, Informedness, YoudenJ 
	s_tuneLength 		= 100				#100 # this is maximum tune length; means it can stop before reaching 100; but errors?? -> 10
	s_repeats 			= 10				#10 ; repeats
	s_number 			= 10 				#10 ; folds
	s_maxCPUCores		= 7					#CPUs-1
	s_trainingsplit		= 0.6				# set to 60% train!
	s_RFE				= TRUE				# Recursive feature extraction

	s_runPowerSplitRatioEstimation = FALSE	# run once!
	
	# Details about ROC curves
	s_ROC_direction = "<"
	s_ROC_Level  = "Class1"

	
	#-----------------------------------------------------------------------------------------------------#
	#							Prompt info
	#-----------------------------------------------------------------------------------------------------#
	cat(paste0("#-----------SETTINGS-----------#\n"))
	if(s_Outcome_type==0){cat("  Chosen class: Impairment outcome\n")}
	if(s_Outcome_type==1){cat("  Chosen class: PDD outcome\n")}
	if(s_Outcome_type==2){cat("  Chosen class: MCIvsPDD\n")}
	if(s_Outcome_type==3){cat("  Chosen class: Subjective complaint\n")}
	if(s_Outcome_type==999){cat("  Chosen class: OLD_Cognitive_Impairment\n")}
	cat(paste0("  Chosen feature subset: ",s_feature_subset,"\n"))
	cat(paste0("  Chosen ML algorithm: ",s_MLmethods,"\n"))
	cat(paste0("  Chosen optimization metric: ",s_metric,"\n"))
	cat(paste0("#------------------------------#\n\n"))
	#-----------------------------------------------------------------------------------------------------#
	#							Folders
	#-----------------------------------------------------------------------------------------------------#
	# Before doing anything, make folders to store all generated results in a logical fashion.
	# First the slowest, then faster (first lancet class, then feature subset)
	# Work from current dir
	
	# check if custom storage place
	if(is.na(SRoot)){
		SRoot = getwd()
	}
	
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(s_Outcome_type==0){temp_Lancet = "Impairment"}
	if(s_Outcome_type==1){temp_Lancet = "PDD"}
	if(s_Outcome_type==2){temp_Lancet = "MCIvsPDD"}
	if(s_Outcome_type==3){temp_Lancet = "Subjective_complaint"}
	if(s_Outcome_type==999){temp_Lancet = "OLD_Cognitive_Impairment"}
	
	# create folder based on uid
	dir.create(path = paste0(SRoot,"/",temp_Lancet,"/"))
	dir.create(path = paste0(SRoot,"/",temp_Lancet,"/",s_feature_subset,"/"))
	dir.create(path = paste0(SRoot,"/",temp_Lancet,"/",s_feature_subset,"/","Plots"))

	# set the working directory to the root folder
	setwd(paste0(SRoot,"/",temp_Lancet,"/",s_feature_subset,"/"))

	
	






	#-----------------------------------------------------------------------------------------------------#
	#							Load raw data and files
	#-----------------------------------------------------------------------------------------------------#
	# load Pheno and groups
	# dataraw_pheno$ClassCol # clinical data based groups
	# dataraw_pheno$Latent_Class # MOCA latent classes
	dataraw_pheno = data.frame(data.table::fread(BaselineClin_new, header=TRUE, stringsAsFactors=FALSE))

	# Load Feature info
	dataraw_feature = data.frame(data.table::fread(FeatureInfo, header=TRUE, stringsAsFactors=FALSE)) 

	# Load alternative classification based on lancer paper
	lancet_class = data.frame(data.table::fread(Lancetdata, header=TRUE, stringsAsFactors=FALSE)) 


	# First remove the first column (as this is V1.. numbers)
	#dataraw_pheno = dataraw_pheno[,-1]


	# Add lancer data to phenodata
	# lancet_class$PATNO==dataraw_pheno$PATNO # true
	dataraw_pheno$Lancet_Class = lancet_class$BinaryPDD

	#-----------------------------------------------------------------------------------------------------#
	#							Set correct groups
	#-----------------------------------------------------------------------------------------------------#


	# define classID from settings
	s_ClassID = which(colnames(dataraw_pheno)==s_ClassID_name)
	
	# Find which groups are of interest
	s_kGroups 			= length(unique(dataraw_pheno[!is.na(dataraw_pheno[,s_ClassID]),s_ClassID])) 				# amount of unique IDs in predictor var: length(unique(dataraw_pheno[,s_ClassID])) !!Without NA!!



	#-----------------------------------------------------------------------------------------------------#
	#							Process dataraw_pheno
	#-----------------------------------------------------------------------------------------------------#
	# dataraw_pheno contains the data and annotation-like features
	# This is split into the raw "data" and "annotation"
	# Data will be saved using a "data" prefix (as data_main)
	# Annotation will be saved as dataraw_pheno (due to existing code)

	# Replace rownames by PATNO, will help greatly
	rownames(dataraw_pheno) = dataraw_pheno$PATNO

	#-----------------------------------------------------------------------------------------------------#
	#New classes (replacing PDD groups)
	#-----------------------------------------------------------------------------------------------------#
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	
	# Impairment outcome
	if(s_Outcome_type==0){
		dataraw_pheno$ClassCol = NA

		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Normal")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "SCD")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "MCI")] <- 1#1
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Dementia")] <- 1#2
		
		dataraw_pheno$ClassCol = as.numeric(dataraw_pheno$ClassCol)
	}

	# PDD outcome
	if(s_Outcome_type==1){
		
		#new mode?
		dataraw_pheno$ClassCol = NA

		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Normal")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "SCD")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "MCI")] <- 0#1
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Dementia")] <- 1#2
		
		dataraw_pheno$ClassCol = as.numeric(dataraw_pheno$ClassCol)
	}
	
		# MCIvsPDD outcome
	if(s_Outcome_type==2){
		dataraw_pheno$ClassCol = NA

		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Normal")] <- NA#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "SCD")] <- NA#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "MCI")] <- 0#1
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Dementia")] <- 1#2
		
		dataraw_pheno$ClassCol = as.numeric(dataraw_pheno$ClassCol)
		
	}
	
	# Subjective complaint outcome
	if(s_Outcome_type==3){
		dataraw_pheno$ClassCol = NA

		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Normal")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "SCD")] <- 1#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "MCI")] <- NA#1
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Dementia")] <- NA#2
		
		dataraw_pheno$ClassCol = as.numeric(dataraw_pheno$ClassCol)
		
	}

		# OLD cognitive impairment outcome
	if(s_Outcome_type==999){
		dataraw_pheno$ClassCol = NA

		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Normal")] <- 0#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "SCD")] <- NA#0
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "MCI")] <- 1#1
		dataraw_pheno$ClassCol[which(dataraw_pheno[,s_ClassID_name] == "Dementia")] <- 1#2
		
		dataraw_pheno$ClassCol = as.numeric(dataraw_pheno$ClassCol)
		
	}


	# Define the 'annotation' cols, but check for the regression case
	# CLASSIFICATION
	temp_annotation_names = c("SITE","PATNO","APPRDX","EVENT_ID","age","Latent_Class","BL_diag","Conversion_group","hemo_below","hemo_above","BaselineCat","Lancet_Class","gen","ClassCol",s_ClassID_name)

	temp_annotation_IDs = which(colnames(dataraw_pheno)%in%temp_annotation_names)


	# remove funny samples (class to NA)
	funnysamples = c("3018","3522","3705","3752","3762","3776","3777","3808","3822")
	dataraw_pheno$ClassCol[match(funnysamples,rownames(dataraw_pheno))] = NA

	# Removing samples which have LOWER THAN 20 MOCA
	LowMocaSamples = which(dataraw_pheno$moca<20)
	dataraw_pheno$ClassCol[match(LowMocaSamples,rownames(dataraw_pheno))] = NA

	# Now add relevant class as "Class" (even regression feature will be called class, dont worry)
	data_main = data.frame(dataraw_pheno[,-temp_annotation_IDs],Class = dataraw_pheno$ClassCol)

	#  dataraw_pheno
	data_all = data.frame(dataraw_pheno[,-temp_annotation_IDs],Class = dataraw_pheno$ClassCol)

	# Create the annotation object
	dataraw_pheno = dataraw_pheno[,c(temp_annotation_IDs)]

	# define classID from settings
	s_ClassID = which(colnames(dataraw_pheno)=="ClassCol")






	#-----------------------------------------------------------------------------------------------------#
	#							ADJUST Modified Schwab & England ADL Score (MSEADLG); this is a cat variable, will not be Z-score-norm
	#-----------------------------------------------------------------------------------------------------#
	# as all the feature scores will range around -3 to 3, the MSEADLG is adjusted to 1-0
	# This is OK because this is a relative score, in which 100(%) is max and 0(%) is min. 
	# Rescaling will give it equal 'weigth' in the ML later, else it might affect too much.
	data_main$MSEADLG = data_main$MSEADLG/100
	data_all$MSEADLG = data_all$MSEADLG/100

	#-----------------------------------------------------------------------------------------------------#
	#							BIN age based on distr
	#-----------------------------------------------------------------------------------------------------#
	# 1 = age < 56
	# 2 = age 56 - 65
	# 3 = age > 65
	dataraw_pheno$agebin = ifelse(dataraw_pheno$age < 56,1,ifelse(dataraw_pheno$age > 65,3,2))
	#-----------------------------------------------------------------------------------------------------#
	#							REMOVE several features/ feature subset selection
	#-----------------------------------------------------------------------------------------------------#
	# Some features are 'total' scores and thus are latent variables, composed out of subfeatures (also in the data).
	# These total features are removed as well as catagorical versions of some features.
	# 
	# @RRR "age" removed -> to annotation
	Remove_features = c("scopa","updrs_totscore","updrs1_score","agediag","rem","ess_cat","TD","gds_cat","td_pigd_old","White","PIGD","rem_cat","BioAge4HAStatic","DNAmAgeSkinBloodClock","DNAmGrimAge")
	
	
	Select_features = NA
	

	# Genetic_Epigenetic features only
	#
	if(s_feature_subset=="All"){
		# select all that are available (and not NA)
		Selected_Subset = dataraw_feature$Label[!is.na(dataraw_feature$Feature_Class)]
	
	
	# If ALL_nonClin = ALL and remove clinical
	}else if(s_feature_subset=="ALLnonClin"){
		# select all that are available (and not NA)
		Selected_Subset = dataraw_feature$Label[!is.na(dataraw_feature$Feature_Class)]
		
		# remove THIS list of clinical cognitive features
		remove_list = c("updrs1_score","NP1COG","bjlot","hvlt_immediaterecall","HVLTRDLY","HVLTREC","HVLTFPRL","hvlt_discrimination","hvlt_retention","lns","VLTANIM","VLTVEG","VLTFRUIT","sft","SDMTOTAL","moca")
		
		Selected_Subset = Selected_Subset[-which(Selected_Subset %in% remove_list)]
	
	}else if(s_feature_subset=="COG"){
		# select all that are available (and not NA)
		Selected_Subset = dataraw_feature$Label[!is.na(dataraw_feature$Feature_Class)]
		
		# remove THIS list of clinical cognitive features
		get_list = c("updrs1_score","NP1COG","bjlot","hvlt_immediaterecall","HVLTRDLY","HVLTREC","HVLTFPRL","hvlt_discrimination","hvlt_retention","lns","VLTANIM","VLTVEG","VLTFRUIT","sft","SDMTOTAL","moca")
		
		Selected_Subset = Selected_Subset[which(Selected_Subset %in% get_list)]
	
	}else if(s_feature_subset=="Clinical_COG"){
		# select all that are available (and not NA)
		Selected_Subset = dataraw_feature$Label[dataraw_feature$Feature_Class=="Clinical"]
		Selected_Subset = Selected_Subset[!is.na(Selected_Subset)] 
		
		# remove THIS list of clinical cognitive features
		remove_list = c("updrs1_score","NP1COG","bjlot","hvlt_immediaterecall","HVLTRDLY","HVLTREC","HVLTFPRL","hvlt_discrimination","hvlt_retention","lns","VLTANIM","VLTVEG","VLTFRUIT","sft","SDMTOTAL","moca")
		
		Selected_Subset = Selected_Subset[-which(Selected_Subset %in% remove_list)]
	
	
	}else if(s_feature_subset=="TEST"){
		# select all that are available (and not NA)
		Selected_Subset = dataraw_feature$Label[!is.na(dataraw_feature$Feature_Class)]
		
		# take this short list
		remove_list = c("SDMTOTAL","moca","sft","hvlt_immediaterecall","HVLTRDLY","VLTVEG","lns","VLTFRUIT")
		
		Selected_Subset = Selected_Subset[which(Selected_Subset %in% remove_list)]
	
	} else {
		# if not all, then select corresponding subset!
		Selected_Subset = dataraw_feature$Label[dataraw_feature$Feature_Class==s_feature_subset]
		Selected_Subset = Selected_Subset[!is.na(Selected_Subset)] 
	}
	
	Select_features = Selected_Subset
	
	

	# always include class and things to stratify for
	# old: Select_features = c(Select_features,"gen","Class") # changed at stratified sampling
	Select_features = c(Select_features,"Class")
	
	# Check if the current sample set has features that should be REMOVED & remove those features
	#check then remove
	if(length(as.numeric(na.omit(match(Remove_features,Select_features))))>0){
		Select_features = Select_features[-as.numeric(na.omit(match(Remove_features,Select_features)))]
	}
	
	# remove the features and overwrite DF
	#OLD: #data_main = data_main[,-which(colnames(data_main)%in%Remove_features)]
	data_main = data_main[,which(colnames(data_main)%in%Select_features)] # new method (form exclusion to inclusion)

	#-----------------------------------------------------------------------------------------------------#
	#							IMPUTATION
	#-----------------------------------------------------------------------------------------------------#
	if(s_Impute){
		# using ONLY PD population. not controls.

		# Define object
		dataoptions = "data_main"

		# load object
		temp_data = get(dataoptions)
		
		# store metrics to be used later
		pMiss <- function(x){sum(is.na(x))/length(x)*100}
		ImputationNA_Fraction_Features = apply(temp_data,2,pMiss)
		ImputationNA_Fraction_Samples = apply(temp_data,1,pMiss)
		
		# generate imputation difference data object
		IputeDiffDF = data.frame(Feature = names(temp_data), Full = NA, PD = NA, NONPD = NA, RANGE_min = NA,RANGE_max = NA,Full_PD=NA,MissingNO=NA)

		# for each column, check and impute NAs
		for(i in 1:dim(temp_data)[2]){

			NAList = is.na(temp_data[,i])
			
			# Next cases
			if(i == which(colnames(temp_data)=="Class")){next}
			if(sum(NAList)==0){next}
			
			# Get cat or Cont
			temp_isCon = dataraw_feature$Type[match(colnames(temp_data)[i],dataraw_feature$Label)]=="CON"
			
			# Continuous
			if(temp_isCon){
				# missingness
				IputeDiffDF[i,"MissingNO"] = sum(is.na(temp_data[dataraw_pheno$APPRDX==1,i]))
				
				# diffs
				IputeDiffDF[i,"Full"] = median(na.omit(temp_data[,i]))
				IputeDiffDF[i,"PD"] = median(na.omit(temp_data[dataraw_pheno$APPRDX==1,i]))
				IputeDiffDF[i,"NONPD"] = median(na.omit(temp_data[dataraw_pheno$APPRDX==2,i]))
				IputeDiffDF[i,"RANGE_min"] = range(na.omit(temp_data[,i]))[1]
				IputeDiffDF[i,"RANGE_max"] = range(na.omit(temp_data[,i]))[2]
				
			
				# Impute value
				temp_data[NAList,i] = median(na.omit(temp_data[dataraw_pheno$APPRDX==1,i]))
				
				
				
			}
			
			# Cat/Ord
			if(!temp_isCon){
				# missingness
				IputeDiffDF[i,"MissingNO"] = sum(is.na(temp_data[dataraw_pheno$APPRDX==1,i]))
				
				# diffs
				IputeDiffDF[i,"Full"] = Mode(na.omit(temp_data[,i]))
				IputeDiffDF[i,"PD"] = Mode(na.omit(temp_data[dataraw_pheno$APPRDX==1,i]))
				IputeDiffDF[i,"NONPD"] = Mode(na.omit(temp_data[dataraw_pheno$APPRDX==2,i]))
				IputeDiffDF[i,"RANGE_min"] = range(na.omit(temp_data[,i]))[1]
				IputeDiffDF[i,"RANGE_max"] = range(na.omit(temp_data[,i]))[2]
				
				# impute value
				temp_data[NAList,i] = Mode(na.omit(temp_data[dataraw_pheno$APPRDX==1,i]))
							
				
			}

			
		}

		#apply(temp_data,2,pMiss)

		# cal diff for IputeDiffDF
		IputeDiffDF[,"Full_PD"] = IputeDiffDF$Full-IputeDiffDF$PD
		IputeDiffDF$propChange = round(IputeDiffDF$Full_PD/(IputeDiffDF$RANGE_max-IputeDiffDF$RANGE_min),2)
		IputeDiffDF$propChange[IputeDiffDF$Full_PD==0] = NA
		
		# Filter to get only changed features
		IputeDiffDF_changed = IputeDiffDF[!is.na(IputeDiffDF$propChange),]
		
		# assign the object to var
		assign(x = "data_main",value = temp_data)

	}

	#-----------------------------------------------------------------------------------------------------#
	#							Power estimation of data; define spit ratio!
	#							This should be run once; it takes LONG
	#-----------------------------------------------------------------------------------------------------#
	if(s_runPowerSplitRatioEstimation){
		TrainTestRatio = func_runPowerSplitRatioEstimation(dataraw_pheno,data_main,s_ClassID_name,s_maxCPUCores,s_MLmethods,SRoot)
	}
	#-----------------------------------------------------------------------------------------------------#
	#							Stratified sampling (REPLACED)
	#-----------------------------------------------------------------------------------------------------#
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
	
	# this is the reduced sample set
	Fullsampleset = a$PATNO

	# Apply the stratified sampling to combined set, getting 70% of 50/50 
	sampleset = a$PATNO %in% splitstackshape::stratified(a, c("ClassCol",s_ClassID_name,"gen","agebin"),size = s_trainingsplit)$PATNO

	##############
	# Added after comments. 
	# basically this part will ensure TEST data will have 1:1 ratio of samples to be able to predict MCC on same scale troughout models. This will reduce TEST sample size, as 'normal' samples are removed. The normal samples are returned to training to still use the samples, but training is becoming more biased. This might be an issue in low effect situations. The idea is that the models will optimize and maximize MCC, the best model will likely (after CV and such) generalize well, and therefore can still achieve MCC of 1 on TEST (and TRAIN). However, TEST can now be truthfully evaluated, and an MCC of 1 is theoretically possible, as it was unable t do so before this change. 
	# CASE0 = normal, CASE1 = "Diseased" just for reference now.
	# ADDED: on/off setting!
	
	if(s_UseStratifiedTest){
	
		# example numbers below follow as im working this out, these are not final numbers and can be ignored.
		#	109 is normal
		current_test_CASE0 = sum(a$ClassCol[!sampleset]==0)
		
		#	17 is Diseased
		current_test_CASE1 = sum(a$ClassCol[!sampleset]==1)
		
		# logic is, of 109 only 17 should remain = 109-17
		# a2 = TEST&CASE0
		a2 = a[!sampleset,][a$ClassCol[!sampleset]==0,]
		
		a2_names = a2$PATNO %in% splitstackshape::stratified(a2, c(s_ClassID_name,"gen","agebin"),size = current_test_CASE1/current_test_CASE0)$PATNO
		
		# TestToTrain&CASE0 filtered # should be 91 trues in this
		a3 = Fullsampleset %in% rownames(a2[!a2_names,])
		
		# add and make logical combined sampleset
		# TRUE = train, FALSE  = TEST
		sampleset = as.logical(sampleset+a3)
	}
	#> table(sampleset )
	#sampleset
	#FALSE  TRUE 
	#   35   281 
	
	# Works out!!!! 
	# this results in higher train, more normal samples in train. 1:1 ratio in TEST
	####################



	#sampleset = a$PATNO %in% dplyr::sample_frac(a, 0.7, fac=c("ClassCol"))$PATNO
	# Define train/test
	subdataraw_pheno = subdataraw_pheno[subdataraw_pheno$PATNO %in% Fullsampleset,]
	subdataraw_pheno$dataset[sampleset]="train"
	subdataraw_pheno$dataset[!sampleset]="test"

	# Check train using
	table(subdataraw_pheno$ClassCol[with(subdataraw_pheno,dataset=="train")])
	table(subdataraw_pheno[,s_ClassID_name][with(subdataraw_pheno,dataset=="train")])

	# Check test using
	table(subdataraw_pheno$ClassCol[with(subdataraw_pheno,dataset=="test")])
	table(subdataraw_pheno[,s_ClassID_name][with(subdataraw_pheno,dataset=="test")])

	trainPATNO = subdataraw_pheno$PATNO[subdataraw_pheno$datase=="train"]
	testPATNO = subdataraw_pheno$PATNO[subdataraw_pheno$datase=="test"]


	#-----------------------------------------------------------------------------------------------------#
	#							GENERATE Z SCORE REF MATRIX
	#-----------------------------------------------------------------------------------------------------#
	if(s_ZscoreNorm){

		# Get control group PATNOs
		#temp_CTRLsamples = dataraw_pheno$APPRDX == 2 # 1 = PD, 2 = CTRL
		#temp_CTRLsamplesnames = dataraw_pheno$PATNO[temp_CTRLsamples]
		#temp_TESTsamplesnames = dataraw_pheno$PATNO[!temp_CTRLsamples]

		# get all datasets
		#temp_datasetlist = ls()[grep(ls(),pattern = "^data_")]
		temp_datasetlist = "data_main"

		# create object to store critial measures
		Zscore_metrics_perdataset = list()
		
		## select every dataset sequentially
		for( i in 1:length(temp_datasetlist)){
			
			# get data object
			temp_data = get(temp_datasetlist[i])

			temp_data = temp_data[match(trainPATNO,rownames(temp_data)),]

			# remove class from data
			temp_ClassID = which(colnames(temp_data)=="Class")
			#temp_data = temp_data[,-temp_ClassID]
			
			# Identify the catagorical columns, drop these; EXCLUDING CLASS
			RemoveVec_list = dataraw_feature$Type[match(colnames(temp_data)[1:(dim(temp_data)[2]-1)],dataraw_feature$Label)]=="CON"
			RemoveVec_list = c(RemoveVec_list,Class=FALSE)	
			
			# Check if PRS is in there, should be evaluated using CTRL data
			PRS_cols = grep(colnames(temp_data),pattern = "_PRS")
			
			# So 3 cases; 1= PRS scaling, 2= Normal scaling, 3= No scaling (so nothing really...).
			# Create a df to accolate space and have good dimentions + names
			Res_df = temp_data
			

			# Define the test(AD/PD) / Control samples
		#	CTRLorder = match(temp_CTRLsamplesnames,rownames(Res_df))
		#	TESTorder = match(temp_TESTsamplesnames,rownames(Res_df))
			
			# Make boxplot for check
			if(s_Saving){
				png(paste0("Plots/",s_MLmethods,"_","Boxplot_Features_before.png"),width = 4000,height = 6700, res = 350)
					boxplot(temp_data[,-temp_ClassID],main=paste0(temp_datasetlist[i],"_Before"))
				dev.off()          
			}
			
			# Make attribute matix
			Attr_matrix= data.frame(Center=matrix(NA,nrow=dim(Res_df)[2],ncol=1),Scale=matrix(NA,nrow=dim(Res_df)[2],ncol=1))
			

			# Build a loop to process the dataframe col by col (Can be parrallelized later)
			for(o in 1:dim(Res_df)[2]){
				# Case 1: PRS scaling
				if(o%in%PRS_cols){
				
					# CTRL added here
					temp_CTRLsamples = dataraw_pheno$APPRDX == 2 # 1 = PD, 2 = CTRL
					temp_CTRLsamplesnames = dataraw_pheno$PATNO[temp_CTRLsamples]
					CTRLorder = match(temp_CTRLsamplesnames,rownames(data_main))
					
							
					# get the scaling based on only CTRLorder
					temp_scaled = attributes(scale(data_main[CTRLorder,o]))
					temp_center = temp_scaled$`scaled:center`
					temp_scale = temp_scaled$`scaled:scale`
					
					# then project the scales using TESTorder
					# Replace values in df
					Res_df[,o] = scale(Res_df[,o],center=temp_center,scale=temp_scale)
		
					# Store the attributes in attribute matrix
					Attr_matrix$Center[o] = temp_center
					Attr_matrix$Scale[o] = temp_scale
					
				
					
				}else{
				
					# Case 2: Normal scaling
					if(RemoveVec_list[o]){
						#print(paste0(colnames(temp_data)[o]," ",RemoveVec_list[o]))
						
						# Replace values in df
						scaling = scale(Res_df[,o])
						Res_df[,o] = scaling
						
						# Get attributes
						temp_scaled = attributes(scaling)
						temp_center = temp_scaled$`scaled:center`
						temp_scale = temp_scaled$`scaled:scale`
						
						# Store the attributes in attribute matrix
						
						Attr_matrix$Center[o] = temp_center
						Attr_matrix$Scale[o] = temp_scale
						
					}
				}
			Attr_matrix$Name[o] = colnames(Res_df)[o]
			Attr_matrix$Typeoff[o] = dataraw_feature$Type[match(Attr_matrix$Name[o],dataraw_feature$Label)]
			}
			
			Zscore_metrics_perdataset[[temp_datasetlist[i]]] = Attr_matrix

			#temp_data = Res_df[,]
			
			# now remove NA classes form main object
			#temp_data = temp_data[!is.na(temp_data$Class),]
			
			if(s_Saving){
				png(paste0("Plots/",s_MLmethods,"_","Boxplot_Features_after.png"),width = 4000,height = 6700, res = 350)
					boxplot(Res_df[,-temp_ClassID],main=paste0(temp_datasetlist[i],"_After"))
				dev.off()          
			}
			
			
			#assign(x = paste0("Combine_",temp_datasetlist[i]),value = temp_data)

			#Zscore_metrics_perdataset
		}
	}

	#-----------------------------------------------------------------------------------------------------#
	#							PROCESS DATA USING REF Z-SCORE MATRIX
	#-----------------------------------------------------------------------------------------------------#


	for(i in 1:dim(data_main)[2]){

		# skip variables not to be normalized
		if(is.na(Zscore_metrics_perdataset$data_main$Center[i])){
			next
		}
		
		# get the corresponding scalar ID
		temp_ID = match(colnames(data_main)[i],Zscore_metrics_perdataset$data_main$Name)
		
		# normalize using metrics of given 
		data_main[,i] = scale(data_main[,i],center=Zscore_metrics_perdataset$data_main$Center[temp_ID],scale=Zscore_metrics_perdataset$data_main$Scale[temp_ID])

	}


	#-----------------------------------------------------------------------------------------------------#
	#							Get relevant cases
	#-----------------------------------------------------------------------------------------------------#
	Combine_data_main = data_main[match(rownames(temp_data_main),rownames(data_main)),]

	if(s_Saving){
		png(paste0("Plots/",s_MLmethods,"_","Boxplot_Features_Final.png"),width = 4000,height = 6700, res = 350)
			par(oma=c(5,2,2,2))
			boxplot(Combine_data_main,las=2)
		dev.off()          
	}
	
	
	# this part was added bc of the meeting for the ML people, wanted a better boxplot to show 
	if(s_Saving){
		png(paste0("Plots/",s_MLmethods,"_","Boxplot_Features_Final_ordered.png"),width = 4000,height = 6700, res = 350)
			par(oma=c(2,10,2,2))
			
			colnames(Combine_data_main)
			featurre_type_order = Zscore_metrics_perdataset$data_main[match(colnames(Combine_data_main),Zscore_metrics_perdataset$data_main$Name),"Typeoff"]
			
			featurre_type_order = factor(featurre_type_order,levels = c("CAT","ORD","CON"))
			
			cols = gsub(pattern = "CON",replacement = "cornflowerblue",gsub(pattern = "ORD",replacement = "orange",gsub(pattern = "CAT",replacement = "red",featurre_type_order)))
			
			boxplot(Combine_data_main[,order(featurre_type_order)],las=2,horizontal = T,border = cols[order(featurre_type_order)])
		dev.off()          
	}


	#-----------------------------------------------------------------------------------------------------#
	#						ML
	#-----------------------------------------------------------------------------------------------------#

	# define Train and Test samples
	TrainPATNO = subdataraw_pheno$PATNO[subdataraw_pheno$dataset=="train"]
	TestPATNO = subdataraw_pheno$PATNO[subdataraw_pheno$dataset=="test"]

	#dataoptions = ls()[grep(x = ls(),pattern="^FE_")]
	dataoptions = "Combine_data_main"
	
	# Create result object
	superresult = list()

	# create temp object to hold all the models untill best has been chosen
	superresult_temp = list()

	# create eval object
	supereval = list()
	supereval_mcc = list()

	#s_iterativeStepsize
	# Evaluate each dataset as stated in dataoptions
	for(o in 1:length(dataoptions)){
		
		# initialize
		Selecteddata = get(dataoptions[o])
		
		# format all data to be numeric
		Selecteddata[,-which(colnames(Selecteddata)=="Class")] = apply(Selecteddata[,-which(colnames(Selecteddata)=="Class")],2,as.numeric)
		
		# remove NAs from classes
		Selecteddata = Selecteddata[!is.na(Selecteddata$Class),]
		
		
		### @RRR ####
		### REMOVING NAS BY OMIT NA, LOOSING SAMPLES HERE ####
		### EDIT: after imputation (above) no losses.	  ####
		
		Selecteddata = na.omit(Selecteddata)
			
		# Keeping 72 of 125 -> IS IMPUTED NOW, NOT DROPPING SAMPLES
		### @RRR ####
		
		# convert numeric class to factor class
		Selecteddata$Class = as.factor(paste0("Class",Selecteddata$Class))
		
		# Multiclass MCC, functioning at 2 levels (normal MCC)
		# make classes have 'good metric'
		
		
		
		# set training / optimisation and control parameters
		fitControl <- trainControl(method = "repeatedcv", number = s_number, repeats = s_repeats, classProb=TRUE,  search = "random", savePredictions = TRUE, summaryFunction = RRtest::MCC) # MCC / twoClassSummary / prSummary
					
		# split data train and test
		temp_data_train = Selecteddata[rownames(Selecteddata) %in% TrainPATNO,]
		temp_data_test = Selecteddata[rownames(Selecteddata) %in% TestPATNO,] # this is actually the validation set!

		# remove invariant columns (causes due to sample selection)
		RelevantIndex = c(which((apply(temp_data_train[,-(dim(temp_data_train)[2])],2,sd,na.rm=T)>0)),which(names(temp_data_train)=="Class"))
		temp_data_train = temp_data_train[,RelevantIndex]

		# detemine traning / testing set
		ClassIndex = which(names(temp_data_train)=="Class")
		Class = temp_data_train[,ClassIndex]

		#Garbage collect
		gc()

		# create temp result frame for scoring each ML method
		df_result = data.frame(Methods = s_MLmethods,TestMCC=NA,TestAUC=NA)
		
		# create temp result list for each dataset
		subresult = list()
		
		# Evaluate each dataset with all ML methods given in s_MLmethods
		for(i in 1:length(s_MLmethods)){
		s_MLmethods_select = s_MLmethods[i]
			
			# run ML engine
			tempres = MLengine(fitControl=fitControl,s_maxCPUCores=s_maxCPUCores,temp_data_train=temp_data_train,s_metric=s_metric,s_MLmethods_select=s_MLmethods_select,s_tuneLength=s_tuneLength,s_kGroups=s_kGroups,temp_data_test=temp_data_test,s_VERBOSE=s_VERBOSE,subresult=subresult,s_MLmethods=s_MLmethods,s_ROC_Level=s_ROC_Level,s_ROC_direction=s_ROC_direction)
			
			#back the data up
			backup_temp_data_train = temp_data_train
			backup_temp_data_test = temp_data_test
			
			#temp_data_train = backup_temp_data_train
			#temp_data_test = backup_temp_data_test
			
			# Recursive Feature Extraction
			if(s_RFE==TRUE){
				while(dim(tempres[[i]]$fit$trainingData)[2]>3){
				
					# (RE)run ML with updated setting
					tempres = MLengine(fitControl,s_maxCPUCores,temp_data_train,s_metric,s_MLmethods_select,s_tuneLength,s_kGroups,temp_data_test,s_VERBOSE,subresult,s_MLmethods,s_ROC_Level,s_ROC_direction)
					#browser()
					#browser(print(TRUE%in%is.nan(tempres[[i]]$importance$importance[[1]])))
					# extra check if model actually generates and is viable for further analysis. Basically variable importance should NOT be NaN.
					if(TRUE%in%is.nan(tempres[[i]]$importance$importance[[1]])){next} # as this is a while loop, next means redo
					
					# get current feature count
					# p also contains CLASS; so it should be p-1
					p= dim(tempres[[i]]$fit$trainingData)[2]
					
					# get evaluation metric to pinpoint best model
					supereval[[(p-1)]] = tempres[[i]]$AUCval_train$auc[1]
					supereval_mcc[[(p-1)]] = tempres[[i]]$resultMCC_TRAIN[s_metric]
					
					# get importance; exception on glmnet (as it miscalcualted, using t-statistic on ordinal and categorical variables)
					if(s_MLmethods != "glmnet"){
						imp = tempres[[i]]$importance$importance
						}else{
						imp = as.matrix(coef(tempres[[i]]$fit$finalModel,s=as.numeric(tempres[[i]]$fit$finalModel$tuneValue[2])))
						
						imp=imp[-which(rownames(imp)=="(Intercept)"),]
						imp = data.frame(imp)
					}
					
					
					
					# get worst performing one
					RemID = which(abs(imp)==min(abs(imp)))[1]
					RemName = rownames(imp)[RemID]
					
					# remove worst feature from train and test set
					temp_data_train = temp_data_train[,-which(colnames(temp_data_train)==RemName)]
		
					temp_data_test = temp_data_test[,-which(colnames(temp_data_test)==RemName)]
					
					superresult_temp[[(p-1)]] = tempres
				}
				# get optimal lsit
				#Optimalrange = rank(-c(as.numeric(supereval[1:length(supereval)])))+1:length(supereval)
				
				
				# average of 5 points
				n=5
				#browser() #@RRR debug
				# chekc if this is needed
				if(sum(as.logical(lapply(supereval,is.null)))>0){
					# get moving/rolling average of MCC and AUC
					for( i_null in max(1,sum(as.logical(lapply(supereval_mcc,is.null)))):1){
						temp_index = which(as.logical(lapply(supereval_mcc,is.null)))[i_null]
						supereval_mcc[temp_index]=min(as.numeric(unlist(supereval_mcc)),na.rm = T)/2
						supereval[temp_index]=min(as.numeric(unlist(supereval)),na.rm = T)/2
					}
				}
				MCC_MA = stats::filter(as.numeric(supereval_mcc), rep(1 / n, n), sides = 2)
				AUC_MA = stats::filter(as.numeric(supereval), rep(1 / n, n), sides = 2)
				
				
				
				# get the combined list; as we need a good model AND performs well... @rrr this can be questioned 'which' is the best
				# so when the probabilities of samples begin to drop, MCC remains the same while AUC decreases. A mix between 2 has been selected...
				updatedranklist = (rank(-MCC_MA) + rank(-AUC_MA))/2+((1:length(MCC_MA))*1)
				
				updatedranklist[is.na(MCC_MA)] = 9999999
				
				# select the lowest rank (best of the 2)
				optimalModel = which(updatedranklist==min(updatedranklist))[1]
				
				# If lowest model is selected, makes no sense; min 2 features
				optimalModel = max(optimalModel,2)
				
				# make a plot indicating the numbers
				plot(as.numeric(supereval_mcc),xlab="Number of features",ylab = paste0("TRAIN ",s_metric,"(black) / AUC(blue)"),ylim=c(0.5,1),type="p",col="black",pch=19,main="RFE optimization curve")#
				points(as.numeric(supereval),col="blue",pch=19)
				lines(MCC_MA,col="black",lwd = 2 )
				lines(AUC_MA,col="blue",lwd = 2 )
				abline(v=optimalModel,lwd = 2 ,col="red")
				
				
				#plot(c(as.numeric(supereval_mcc[1:length(supereval_mcc)])),xlab="Number of features",ylab = "TRAIN MCC",pch=19,ylim=c(0.5,1))
				#points(c(as.numeric(supereval[1:length(supereval)])),col="blue")
				#abline(v=optimalModel)
				
				tempres = superresult_temp[[optimalModel]]
				tempres = list(model=tempres,RFE=list(mcc=supereval_mcc,auc=supereval,mcc_MA = MCC_MA, auc_MA = AUC_MA, optimal = optimalModel, rankorder = updatedranklist))
			}
			
			#Restore data
			temp_data_train = backup_temp_data_train
			temp_data_test = backup_temp_data_test
		
		} # end of per i (ML method)	
		# ML + store data in super-store object
		superresult[dataoptions[o]] = list(tempres)
		

		
		#Garbage collect
		gc()
	}

	#-----------------------------------------------------------------------------------------------------#
	#							Get all P for all samples
	#-----------------------------------------------------------------------------------------------------#

	# Z-score
	for(i in 1:dim(data_all)[2]){

		# match the feature in all and select the correct centere
		matchedID = match(colnames(data_all)[i],Zscore_metrics_perdataset$data_main$Name)

		# if no match
		if(is.na(matchedID)){
			next
		}else{
			selected_i = matchedID
		}


		# skip variables not to be normalized
		if(is.na(Zscore_metrics_perdataset$data_main$Center[selected_i])){
			next
		}
		
		# get the corresponding scalar ID
		temp_ID = match(colnames(data_all)[i],Zscore_metrics_perdataset$data_main$Name)
		
		# normalize using metrics of given 
		data_all[,i] = as.numeric(scale(data_all[,i],center=Zscore_metrics_perdataset$data_main$Center[temp_ID],scale=Zscore_metrics_perdataset$data_main$Scale[temp_ID]))

	}

	# select coefnames from data
	CoefNames= superresult$Combine_data_main$model[[1]]$fit$coefnames

	# select relevant cols
	data_all_2 = data_all[,CoefNames]

	# format all cols to numeric
	data_all_2 = apply(data_all_2,2,as.numeric)
	rownames(data_all_2) = rownames(data_all)


	##### impute data
	# load object
	pMiss <- function(x){sum(is.na(x))/length(x)*100}
	#apply(temp_data,2,pMiss)

	# for each column, check and impute NAs
	for(i in 1:dim(data_all_2)[2]){

		NAList = is.na(data_all_2[,i])
		
		# Next cases
		#if(i == which(colnames(data_all_2)=="Class")){next}
		if(sum(NAList)==0){next}
		
		# Get cat or Cont
		temp_isCon = dataraw_feature$Type[match(colnames(data_all_2)[i],dataraw_feature$Label)]=="CON"

		if(temp_isCon){
			data_all_2[NAList,i] = median(na.omit(data_all_2[,i]))
		}
		if(!temp_isCon){
			data_all_2[NAList,i] = Mode(na.omit(data_all_2[,i]))
		}

		
	}

	# predict using model
	predictions_all_p <- predict(superresult$Combine_data_main$model[[1]]$fit, data_all_2, na.action = na.omit, type="prob")


	rownames(predictions_all_p)= rownames(data_all_2)

	#save(predictions_all_p,file = "ProbOfAllSamples.Rdata")

	#-----------------------------------------------------------------------------------------------------#
	#							Check Imputation bias
	#-----------------------------------------------------------------------------------------------------#

	# Before the NA fraction was calculated per FEATURE and SAMPLE
	# Looking into SAMPLES here

	# The idea is that NA fraction and prediction probability are NOT related (in a linear or nonlinear fashion, random is OK)
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_","Imputation_Samples_boxplot.png"),width = 4000,height = 4000, res = 350)
				boxplot(predictions_all_p[,2]~round(ImputationNA_Fraction_Samples),las=2,main="Imputation affecting prediction probability \n(SAMPLES)",xlab="NA fraction(%)", ylab="Prediction probability Class 1 (p)")
			dev.off()
	}

	# Now check if the selected feature's importance are affected
	superresult$Combine_data_main$model[[1]]$importance$importance
	ImputationNA_Fraction_Features_subset = ImputationNA_Fraction_Features[match(rownames(superresult$Combine_data_main$model[[1]]$importance$importance),names(ImputationNA_Fraction_Features))]

	# Boxplot of feature importance
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_Imputation_Features_boxplot.png"),width = 4000,height = 4000, res = 350)
				boxplot(superresult$Combine_data_main$model[[1]]$importance$importance[[1]]~round(ImputationNA_Fraction_Features_subset,2),las=2,main="Imputation affecting Variable Importance\n(FEATURES)",xlab="NA fraction(%)", ylab="Variable Importance (%)")
			dev.off()
	}	

	#-----------------------------------------------------------------------------------------------------#
	#							Compute CI for AUC and MCC test
	#-----------------------------------------------------------------------------------------------------#
	pROC::ci.auc(superresult$Combine_data_main$model[[1]]$AUCval_test,direction=s_ROC_direction) 

	#, superresult$Combine_data_main$model[[1]]$predictions_test_p
	#-----------------------------------------------------------------------------------------------------#
	#							Rick's experimental testing areaaaa~ *ding*
	#-----------------------------------------------------------------------------------------------------#
	# mainly plotting
		#-----------------------------------------------------------------------------------------------------#
		#							PLOTTING: ggplot preparation
		#-----------------------------------------------------------------------------------------------------#
		## packages
		#library(tidyverse)
		#library(ggsci)
		#library(showtext)
		#library(cowplot)
		#library(pROC)


		#general check

		# Training, should be high by default
		a= data.frame(obs=superresult$Combine_data_main$model[[1]]$fit$trainingData$.outcome, pred=superresult$Combine_data_main$model[[1]]$predictions_train)
		RRtest::MCC(a)
		table(a)

		# test, should be high if learned correctly
		a= data.frame(obs=temp_data_test$Class, pred=superresult$Combine_data_main$model[[1]]$predictions_test)
		RRtest::MCC(a)
		table(a)

		#-----------------------------------------------------------------------------------------------------#
		#							Varimp
		#-----------------------------------------------------------------------------------------------------#
		# Show
		#plot(superresult$Combine_data_main$model[[1]]$importance)
		g_plot = plot(superresult$Combine_data_main$model[[1]]$importance)

		# Save
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_VarImp_Plot.png"),width = 2000,height = 4000, res = 350)
				plot(g_plot)
			dev.off()
		}

		#-----------------------------------------------------------------------------------------------------#
		#							PLOTTING: ROC
		#-----------------------------------------------------------------------------------------------------#
		#define object to plot and calculate AUC
		Pred_p = superresult$Combine_data_main$model[[1]]$predictions_test_p
		Obs_C = temp_data_test$Class
		ResROC = pROC::roc(Obs_C, Pred_p[, s_ROC_Level],direction=s_ROC_direction)

		auc <- round(ResROC$auc,4)
		
		# generate CM table
		CMtable = data.frame(obs=temp_data_test$Class, pred=superresult$Combine_data_main$model[[1]]$predictions_test)
		CMtable = table(CMtable)
		
		#create ROC plot
		# png("ROC_Plot.png", width = 2500, height = 2500,res = 250)
		#ggroc(ResROC, colour = 'steelblue', size = 2) +
		#  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))+
		#  theme_cowplot(24)+				    
		#  labs(x = "False Positive Rate (1-Specificity)", 
		#		   y = "True Positive Rate (Sensitivity)")+
		#		   geom_abline(slope = 1,intercept = 1,lty=2)
		#		  # +
		#		  # geom_text(data="1", aes(y="1", label="1"))
		#  # dev.off()
		  
			g_plot = ggroc(ResROC, colour = 'steelblue', size = 2) +
			  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))+
			  theme_cowplot(24)+				    
			  labs(x = "False Positive Rate (1-Specificity)", 
			   y = "True Positive Rate (Sensitivity)")+
			   geom_abline(slope = 1,intercept = 1,lty=2)+			   
			   annotate(geom = "table", x = 0.21, y = 0.05, label = data.frame(CM =rownames(CMtable),as.data.frame.matrix(CMtable)), vjust = 1, hjust = 0)
		   
		  
		# Save
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_ROC_Plot.png"),width = 4000,height = 4000, res = 350)
			   plot(g_plot)
		   dev.off()
	   }
	 
		#-----------------------------------------------------------------------------------------------------#
		#							RFE plot
		#-----------------------------------------------------------------------------------------------------#
		# make a plot indicating the numbers
		#plot(as.numeric(superresult$Combine_data_main$RFE$mcc),xlab="Number #of features",ylab = "TRAIN MCC(black) / #AUC(blue)",ylim=c(0.5,1),type="p",col="black",pch=19,main="RFE #optimization curve")#
		#points(as.numeric(superresult$Combine_data_main$RFE$auc),col="blue",pch#=19)
		#lines(superresult$Combine_data_main$RFE$mcc_MA,col="black",lwd = 2 )
		#lines(superresult$Combine_data_main$RFE$auc_MA,col="blue",lwd = 2 )
		#abline(v=superresult$Combine_data_main$RFE$optimal,lwd = 2 ,col="red")
		
			# Save
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_RFE_plot.png"),width = 3000,height = 3000, res = 350)
				plot(as.numeric(superresult$Combine_data_main$RFE$mcc),xlab="Number of features",ylab = paste0("TRAIN ",s_metric,"(black) / AUC(blue)"),ylim=c(0.5,1),type="p",col="black",pch=19,main="RFE optimization curve")#
				points(as.numeric(superresult$Combine_data_main$RFE$auc),col="blue",pch=19)
				lines(superresult$Combine_data_main$RFE$mcc_MA,col="black",lwd = 2 )
				lines(superresult$Combine_data_main$RFE$auc_MA,col="blue",lwd = 2 )
				abline(v=superresult$Combine_data_main$RFE$optimal,lwd = 2 ,col="red")
			dev.off()
		}

		#-----------------------------------------------------------------------------------------------------#
		#							Josh's Plotting
		#-----------------------------------------------------------------------------------------------------#
						
		tophitsmaxamount = 10
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
		dataraw_pheno_new = read.csv(BaselineClin_new, header=TRUE, stringsAsFactors=FALSE)

		#-----------------------------------------------------------------------------------------------------#
		#New classes (replacing PDD groups)
		#-----------------------------------------------------------------------------------------------------#
		# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
		
		# Impairment outcome
		if(s_Outcome_type==0){
			dataraw_pheno_new$ClassCol = NA

			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 1#1
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
			
			dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
		}

		# PDD outcome
		if(s_Outcome_type==1){
			#new mode?
			dataraw_pheno_new$ClassCol = NA

			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 0#1
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
			
			dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
		}
		
			# MCIvsPDD outcome
		if(s_Outcome_type==2){
			dataraw_pheno_new$ClassCol = NA

			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- NA#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- NA#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 0#1
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
			
			dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
			
		}


		# Subjective complaint outcome
		if(s_Outcome_type==3){
			dataraw_pheno_new$ClassCol = NA

			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 1#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- NA#1
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- NA#2
			
			dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
			
		}

			# OLD cognitive impairment outcome
		if(s_Outcome_type==999){
			dataraw_pheno_new$ClassCol = NA

			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- NA#0
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 1#1
			dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
			
			dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
			
		}



		#dataraw_pheno_new[which(dataraw_pheno_new$Conversion_group == "Normal_PDD" |dataraw_pheno_new$Conversion_group == "MCI_PDD" ),"ClassCol"] <- 1
		dataraw_pheno_new <- dataraw_pheno_new[-which(is.na(dataraw_pheno_new$ClassCol)),]
		dataraw_pheno_new$Class <- as.factor(dataraw_pheno_new$ClassCol)
		levels(dataraw_pheno_new$Class) <- c("Stable","Dementia \nConverter ")
		dataraw_pheno_new$count <-  1 # Count variable for stacked bar charts

		# Below for loop takes the top 20 feature and creates either box/violin plots to or stacked bars
		plot_list <- list()
		for(x in 1:tophitsamount){
		  if(df[x,"Type"] == "CON"){
		  
			plot_list[[x]]  <- ggplot(dataraw_pheno_new, aes_string(x = as.character(df$Names[x]), y = "Class", fill = "Class")) +
				geom_violin(alpha = 0.3, color = NA)+
				geom_boxplot(width = 0.2, lwd = 1)+
				theme_cowplot()+
				theme(legend.position = "none")+
				ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
				ylab("")+
				xlab("")+
				scale_fill_manual(values = c("green2","red2"))
			}
		  else{
			
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
		
		# Save
		if(s_Saving){
			png(paste0("Plots/",s_MLmethods,"_Group_differences.png"),width = 4000,height = 6700, res = 350)
				plot(g_plot)
			dev.off()          
		}
		
		#ggsave(name, plot = Plot, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
		#browser() #@RRRdebug
	
		
	#-----------------------------------------------------------------------------------------------------#
	#							Save session
	#-----------------------------------------------------------------------------------------------------#
	
	# Add saving settings to superresult
	# gather all settings and dump in var
	allSettings  = ls(pattern="^s_")
	temp = list()
	for(o in 1:length(allSettings)){
		if(allSettings[o]=="s_PhenoDataFrame"){next} # skip for now @RRR
		tempset = allSettings[o]
		tempval = NA
		tempval = get(tempset)
		#temp[o,1] = tempset
		temp[[tempset]] = tempval
	}
	superresult$settings= temp
	rm(temp)
	
	# first remove all data objects (as else we copy our data for every model we run (40x?) so 1 GB becomes 40+GB...)
	#@RRR
	remove_list=c()
	rm(list=remove_list)
	
	if(s_Saving){
		# Full image
		Savename = paste0(SRoot,"/",temp_Lancet,"/",s_feature_subset,"/",s_MLmethods,"_image.RData")
		save(list = ls(all.names = TRUE), file = Savename, envir = environment()) # save.image is only global enviroment (which is not the place of all stored information)
		
		# Model
		Savename = paste0(SRoot,"/",temp_Lancet,"/",s_feature_subset,"/",s_MLmethods,"_model.RData")
		save(list = "superresult",file = Savename)
	}
	
	#-----------------------------------------------------------------------------------------------------#
	#							Clear all items for next run
	#-----------------------------------------------------------------------------------------------------#
	rm(list=ls())

}


#-----------------------------------------------------------------------------------------------------#
#							More info
#-----------------------------------------------------------------------------------------------------#
# Part of the "Machine Learning-based Prediction of Cognitive Outcomes in de novo Parkinson's Disease" project. See https://github.com/Rrtk2/PPMI-ML-Cognition-PD/blob/main/README.md for licencing and more info.