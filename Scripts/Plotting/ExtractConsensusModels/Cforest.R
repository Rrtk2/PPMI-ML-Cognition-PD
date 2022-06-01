#-----------------------------------------------------------------------------------------------------#
#							Example tree 307
#-----------------------------------------------------------------------------------------------------#


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
			
			# remove all
			#rm(list = ls()[grep(ls(),pattern = "^ROC_",invert = T)])
			gc()
		}

	}


}
#-----------------------------------------------------------------------------------------------------#
#							text
#-----------------------------------------------------------------------------------------------------#
library(party)
y <- superresult$Combine_data_main$model[[1]]$fit$finalModel

plottree = function(i){
	tr <- party:::prettytree(y@ensemble[[i]], names(y@data@get("input")))
	plot(new("BinaryTree", tree=tr, data=y@data, responses=y@responses))
}


#superresult$Combine_data_main$model[[1]]$fit$finalModel@ensemble[[500]][[9]][[9]][[9]][[7]]

# [[5]] [[1]] is name index
# [[8]]	[[5]] [[1]] to left node name index
# [[9]] [[5]] [[1]] to right node name index
# [[9]] [[9]] [[5]] [[1]] to right right node name index
# [[7]] is Percentages, ratio; Contrpol/Case
#AKA
# 51 is index
# 9 is move right
# 8 is move left
# 7 is ratio
y = superresult$Combine_data_main$model[[1]]$fit$finalModel
for(i in 1:500){

	for( o  in 5){
	#y@ensemble[[i]][[o]][[1]]
	if(is.null(y@ensemble[[i]][[o]][[1]])){next}
		if(y@ensemble[[i]][[o]][[1]] == 18){print(paste0(i,"_",o));plottree(i)}
	}
	
}


resdf = data.frame(T=NA,TL=NA,TR=NA,TLR=NA,TRL=NA,TLL=NA,TRR=NA)

for(i in 1:500){

	#for( o  in 5){
		resdf[i,"T"] = if(is.null(y@ensemble[[i]][[5]][[1]])){NA}else{y@ensemble[[i]][[5]][[1]]} 
		resdf[i,"TL"] = if(is.null(y@ensemble[[i]][[8]][[5]][[1]])){NA}else{y@ensemble[[i]][[8]][[5]][[1]]} 
		resdf[i,"TLL"] = if(is.null(y@ensemble[[i]][[8]][[8]][[5]][[1]])){NA}else{y@ensemble[[i]][[8]][[8]][[5]][[1]]}
		resdf[i,"TLR"] = if(is.null(y@ensemble[[i]][[8]][[9]][[5]][[1]])){NA}else{y@ensemble[[i]][[8]][[9]][[5]][[1]]} 
		resdf[i,"TR"] = if(is.null(y@ensemble[[i]][[9]][[5]][[1]])){NA}else{y@ensemble[[i]][[9]][[5]][[1]]} 
		resdf[i,"TRL"] = if(is.null(y@ensemble[[i]][[9]][[8]][[5]][[1]])){NA}else{y@ensemble[[i]][[9]][[8]][[5]][[1]]} 
		resdf[i,"TRR"] = if(is.null(y@ensemble[[i]][[9]][[9]][[5]][[1]])){NA}else{y@ensemble[[i]][[9]][[9]][[5]][[1]]} 
	#}
	
}

table(names(y@data@get("input"))[resdf$T])

resdf2 = resdf
resdf2$T = names(y@data@get("input"))[resdf$T]
resdf2$TL = names(y@data@get("input"))[resdf$TL]
resdf2$TR = names(y@data@get("input"))[resdf$TR]
resdf2$TLL = names(y@data@get("input"))[resdf$TLL]
resdf2$TLR = names(y@data@get("input"))[resdf$TLR]
resdf2$TRR = names(y@data@get("input"))[resdf$TRR]
resdf2$TRL = names(y@data@get("input"))[resdf$TRL]


which(resdf2$T=="SDMTOTAL" & resdf2$TL == "sft" & resdf2$TR == "hvlt_immediaterecall")
which(resdf2$T=="sft" & resdf2$TL == "SDMTOTAL" & resdf2$TR == "HVLTRDLY")

n_top = "hvlt_immediaterecall"
n_1a = "HVLTRDLY"
n_1b = "SDMTOTAL"
which(resdf2$T==n_top & (resdf2$TR == n_1a | resdf2$TR == n_1b ) & (resdf2$TL == n_1a | resdf2$TL == n_1b ))



#350
# 124 366 370 400 423 

plottree(366)
plottree(217) 
 
 
SDMTOTAL             100.000	18
hvlt_immediaterecall  62.612	5
sft                   54.234	15
HVLTRDLY              47.855
lns                   36.758
ptau_ab               33.496
moca                  25.409
HVLTREC               21.497
tau_ab                20.643
VLTVEG                19.307	13
cg13953978            16.962
upsit                 13.958
hvlt_retention        13.719
hvlt_discrimination   11.727	8
VLTFRUIT              11.501
ageonset              11.172
NP1COG                10.794
cg26272088            10.432
stai                   9.409
bjlot                  9.360




y@predict_response()
apply(y@data@get("input")[y@predict_response()=="Class0",],2,mean)-apply(y@data@get("input")[y@predict_response()=="Class1",],2,mean)

#-----------------------------------------------------------------------------------------------------#
#							New plotting method
#-----------------------------------------------------------------------------------------------------#
library(tidyverse)
library(party)
library(partykit)
library(moreparty)
y <- superresult$Combine_data_main$model[[1]]$fit$finalModel

surro <- SurrogateTree(y, maxdepth=3)

surro$r.squared %>% round(3)
plot(surro$tree, inner_panel=node_inner(surro$tree,id=FALSE,pval=FALSE), terminal_panel=node_boxplot(surro$tree,id=FALSE), gp=gpar(cex=0.6), ep_args=list(justmin=15))



  
pdf(paste0(ROC_Rootdir,"/SurroTree_SUP_",format(Sys.time(), "%M%S_%d%m%y"),".pdf"), width = 7, height = 5) # Open a new pdf file
	plot(surro$tree, inner_panel=node_inner(surro$tree,id=FALSE,pval=FALSE), terminal_panel=node_boxplot(surro$tree,id=FALSE), gp=gpar(cex=0.6), ep_args=list(justmin=15))


dev.off()
