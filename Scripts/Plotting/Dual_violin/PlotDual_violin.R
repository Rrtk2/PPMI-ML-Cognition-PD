#-----------------------------------------------------------------------------------------------------#
#							Dual violin plotter script
#-----------------------------------------------------------------------------------------------------#
#generating all images in selected folder
library(gghalves)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)



#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#

# general settings
DV_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results" # results dir
s_tophitsmaxamount = 9999 # big number means all; less means no of features

# enable shaply order
# NOTE: 'ALL' models only, 'OwnClass' models only
s_enable_shap_order = FALSE

# Feature info, Cat/ORD or Cont
#FeatureInfo = "C:/DATA STORAGE/Projects/PPMI/Data RAW/FactorLabels_080721.csv"

# settings plot
s_violin_width = 0.3
s_boxplot_width = s_violin_width/5
s_violin_alpha = 0.3
s_nudge = 0.000

# Save image
s_Aspect_ratio = c(10,2) #10:2 (this looks good)
s_baseValue = 190 #190 (190 per row)
s_TargerDir = "C:/DATA STORAGE/Projects/PPMI/Results/Images/"


#-----------------------------------------------------------------------------------------------------#
#							1 load + 2 Rename
#-----------------------------------------------------------------------------------------------------#

# load data example
#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/Clinical/rf_image.RData")
#load("C:/DATA STORAGE/Projects/PPMI/Results/OwnClass/All/cforest_image.RData")

i_class=0;temp_Outcome = ifelse(i_class,"LancetClass","OwnClass");o_subset="All";p_algo="rf"

# Class
for( i_class in c(0,1,2,3)){ 
	# 0 = Impairment outcome, 1 = PDD outcome, 2 = MCIvsPDD
	if(i_class==0){temp_Outcome = "Impairment"}
	if(i_class==1){temp_Outcome = "PDD"}
	if(i_class==2){temp_Outcome = "MCIvsPDD"}
	if(i_class==3){temp_Outcome = "Subjective_complaint"}
	
	# Subset
	for( o_subset in c("All","Clinical","Serum","Genetic_Epigenetic","ALLnonClin")){ #c("All","Clinical","Serum","Genetic_Epigenetic")

		# Machine learning algorithm
		for( p_algo in c("cforest","svmLinear","glmnet", "rf")){ # svmLinear/glmnet problems ; c("cforest","svmLinear","glmnet", "rf")
			
			# check if shap order needs to be used
			if(s_enable_shap_order){
				# NOTE: 'ALL' models only, 'OwnClass' models only
				if(i_class==1){next}
				if(o_subset!="All"){next}
				# load the shaply generated values to get the order of features
				load(paste0("C:/DATA STORAGE/Projects/PPMI/Results/Images/Shapley 9-9-21/","Shap_adjusted.Rdata"))
			}
			
			# put load data under check if shap order only; is waay faster
			load(paste0(DV_Rootdir,"/",temp_Outcome,"/",o_subset,"/",p_algo,"_image.RData"))
			#-----------------------------------------------------------------------------------------------------#
			#							Special case of overwrite, due to different naming structure of current and past laptop
			#-----------------------------------------------------------------------------------------------------#
			Lancetdata = "file://C:/DATA STORAGE/Projects/PPMI/Data RAW/BinaryPDDlist.csv"
			
			
			#-----------------------------------------------------------------------------------------------------#
			#							Algorithm
			#-----------------------------------------------------------------------------------------------------#

			tophitsamount = min(dim(superresult$Combine_data_main$model[[1]]$importance$importance)[1],s_tophitsmaxamount)
			#dataoptions = "Combine_data_main"

			# check if varimp already exists else remove
			if(exists("temp_Varimp")){rm("temp_Varimp")}
			
			# Check if temp_Varimp will be from svmlinear, as it will induce problems (results multiple columns, not 1 col called Overall). Patching here
			if(p_algo=="svmLinear"){
				temp_Varimp=data.frame(Overall=superresult$Combine_data_main$model[[1]]$importance$importance[[1]],row.names=rownames(superresult$Combine_data_main$model[[1]]$importance$importance))
				
				}else{
				
				#temp_data = get(dataoptions)
				temp_Varimp = superresult$Combine_data_main$model[[1]]$importance$importance
			}
			

			
			
			if(s_enable_shap_order){
				temp_Varimp$Overall = Shap_adjusted[match(rownames(temp_Varimp),Shap_adjusted$Feature),p_algo]
			}
			
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

			# Load Feature info
			dataraw_feature = data.frame(data.table::fread("C:/DATA STORAGE/Projects/PPMI/Data RAW/FactorLabels_080721.csv", header=TRUE, stringsAsFactors=FALSE)) 

			# Add additional data labels from the feature labels file
			# Add variable information
			df$Type <- NULL
			#df_Full_Label <- NULL
			for(x in df$Names){
			  df[which(df$Names == x),"Type"] <- dataraw_feature[which(dataraw_feature$Label == x),"Type"]
			  df[which(df$Names == x),"Full_Label"] <- dataraw_feature[which(dataraw_feature$Label == x),"Full_Label"]
			}

			# if using the model instead of the image, this needs to be ran
			if(FALSE){
				# Re-load the raw pheno file and subset relevant groups
				dataraw_pheno_new = read.csv("file://C:/DATA STORAGE/Projects/PPMI/Data RAW/Baseline_EpigeneticsAdded_140721.csv", header=TRUE, stringsAsFactors=FALSE)

				s_ClassID_name 		= "Longitudinal_diag"
			
				# Impairment outcome
				if(i_class==0){
					dataraw_pheno_new$ClassCol = NA

					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 0#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 1#1
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
					
					dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
				}

				# PDD outcome
				if(i_class==1){
					
					#new mode?
					dataraw_pheno_new$ClassCol = NA

					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 0#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 0#1
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
					
					dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
				}
				
					# MCIvsPDD outcome
				if(i_class==2){
					dataraw_pheno_new$ClassCol = NA

					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- NA#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- NA#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- 0#1
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- 1#2
					
					dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
					
				}
				
				# Subjective complaint outcome
				if(i_class==3){
					dataraw_pheno_new$ClassCol = NA

					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Normal")] <- 0#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "SCD")] <- 1#0
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "MCI")] <- NA#1
					dataraw_pheno_new$ClassCol[which(dataraw_pheno_new[,s_ClassID_name] == "Dementia")] <- NA#2
					
					dataraw_pheno_new$ClassCol = as.numeric(dataraw_pheno_new$ClassCol)
					
				}



					#
					#				# make classes by defined class	of interest
					#				if(s_Lancet_classes==0){
					#					dataraw_pheno_new$PDD_Group = NA
					#
					#					#dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == #"Normal")] <- 0#0
					#					#dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == #"MCI")] <- 1#1
					#					#dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$Longitudinal_diag == #"Dementia")] <- 1#2
					#					
					#					dataraw_pheno_new$PDD_Group = #as.numeric(dataraw_pheno_new$PDD_Group)
					#				}
					#
					#
					#				if(s_Lancet_classes==1){
					#					# Load alternative classification based on lancer paper
					#					lancet_class = #data.frame(data.table::fread(Lancetdata, header=TRUE, #stringsAsFactors=FALSE)) 
					#
					#					dataraw_pheno_new$Lancet_Class = lancet_class$BinaryPDD
					#					dataraw_pheno_new$PDD_Group = #dataraw_pheno_new$Lancet_Class
					#
					#					#dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == #"NonDementia")] <- 0#0
					#					
					#					#dataraw_pheno_new$PDD_Group[which(dataraw_pheno_new$PDD_Group == #"Dementia")] <- 1#0
					#					
					#					dataraw_pheno_new$PDD_Group = #as.numeric(dataraw_pheno_new$PDD_Group)
					#					
					#				}



				#dataraw_pheno_new[which(dataraw_pheno_new$Conversion_group == "Normal_PDD" |dataraw_pheno_new$Conversion_group == "MCI_PDD" ),"PDD_Group"] <- 1
				dataraw_pheno_new <- dataraw_pheno_new[-which(is.na(dataraw_pheno_new$ClassCol)),]
				dataraw_pheno_new$Class <- as.factor(dataraw_pheno_new$ClassCol)
							
				
				if(i_class==0){
					levels(dataraw_pheno_new$Class) <- c("Stable","Impaired")
				}
				if(i_class==1){
					levels(dataraw_pheno_new$Class) <- c("Stable","Dementia \nConverter")
				}
				if(i_class==2){
					levels(dataraw_pheno_new$Class) <- c("MCI","PDD")
				}
				if(i_class==3){
					levels(dataraw_pheno_new$Class) <- c("Stable","Subjective \nComplaint")
				}
				
				
				dataraw_pheno_new$count <-  1 # Count variable for stacked bar charts
			
			}
			
			
			
			# add in class with inverse factor priority; starting with dementia, then normal. This is neede dfor bar plots to hav stable in 1st position; dont know why its being so stubborn!
			dataraw_pheno_new$ClassInvPrio = factor(dataraw_pheno_new$Class,levels =c( "Dementia \nConverter ","Stable"))
	
	



			# uses dataraw_pheno_new (data raw) & df (top importance list and such)
			#dataraw_pheno_new

			dat1 = dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",]
			dat2 = dataraw_pheno_new[dataraw_pheno_new$Class=="Dementia \nConverter ",]




			plot_list <- list()

			for(x in 1:tophitsamount){
				if(df[x,"Type"] == "CON"){
						  
					# get y ranges
					Yrange = range(c(dataraw_pheno_new[,as.character(df$Names[x])]))
				  

					# make figure
					plot_list[[x]] <- ggplot(data=dat1, aes_string(x=1, y=as.character(df$Names[x]),fill="Class",color="Class")) +
						# right (up) part
						geom_half_violin(side=c("r"),alpha = s_violin_alpha,width = s_violin_width,nudge=s_nudge)+
						geom_half_boxplot(side=c("r"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "green4",errorbar.length=1)+
						# left (down) part
						geom_half_violin(data=dat2,side=c("l"),alpha = s_violin_alpha, width = s_violin_width,nudge=s_nudge)+
						geom_half_boxplot(data=dat2,side=c("l"),width = s_boxplot_width, lwd = 1,nudge=s_nudge,color="black",outlier.colour = "red4",errorbar.length=1)+
						# additionals
						ylim(Yrange[1]*0.95, Yrange[2]*1.05)+
						coord_flip()+
						theme_cowplot()+
						theme(legend.position = "none")+
						theme(axis.ticks.y=element_blank())+
						theme(axis.text.y=element_blank())+
						ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
						ylab("")+
						xlab("")+
						scale_fill_manual(values = c("red2","green2"))+
						scale_color_manual(values = c("red3","green3"))# makes the contrast slightly better


				}else{

						
					#start
					dataraw_pheno_new[,as.character(df$Names[x])] <- as.factor(dataraw_pheno_new[,as.character(df$Names[x])])
					
					
					# check if inbalance (num) in colors to be labeled and account for this
					n_cols_stable = length(unique(as.numeric(na.omit(dataraw_pheno_new[dataraw_pheno_new$Class=="Stable",as.character(df$Names[x])]))))
					n_cols_PDD = length(unique(as.numeric(na.omit(dataraw_pheno_new[dataraw_pheno_new$Class!="Stable",as.character(df$Names[x])]))))
					
					# max found colors
					ncolors = max(n_cols_PDD,n_cols_stable)
					
					Reds = RColorBrewer ::brewer.pal(ncolors, "Reds")
					Greens = RColorBrewer ::brewer.pal(ncolors, "Greens")
					Grays = RColorBrewer ::brewer.pal(ncolors, "Greys")
					
					if(ncolors<3){
						Reds = Reds[c(1,2)]
						Greens = Greens[c(1,2)]
						Grays = Grays[c(1,2)]		
					}

					Colorlist = c(Greens[1:n_cols_stable],Reds[1:n_cols_PDD])

					#@RRR omitting NAs would be a possible good idea here!!!
					Raw_plot = ggplot(dataraw_pheno_new, aes_string(x="ClassInvPrio",y="count", fill=paste0("Class:",as.character(df$Names[x])) )) + 
								geom_bar(position="fill",stat="identity")+
								coord_flip()+
								ggtitle("")+
								xlab("")+
								ylab("")+
								theme_cowplot()+
								#scale_fill_manual(values = c("red2","green2","green3"))+
								#scale_fill_brewer(palette = "Reds")+
								ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
								scale_fill_manual(values = Colorlist)+
								theme(legend.position="none")+
								theme(axis.ticks.y=element_blank())+
								theme(axis.text.y=element_blank())
								
								
								
								
					legend_plot = ggplot(dataraw_pheno_new, aes_string(fill=paste0(as.character(df$Names[x])), y="count", x="Class")) + 
								geom_bar(position="fill", stat="identity")+
								coord_flip()+
								ggtitle("")  +
								xlab("")+
								ylab("")+
								theme_cowplot()+
								#scale_fill_manual(values = c("red2","green2","green3"))+
								#scale_fill_brewer(palette = "Reds")+
								ggtitle(paste(df$Full_Label[x]," ","(",signif(df$VarImp[x],3),")",sep = ""))+
								theme(legend.position='right')+
								scale_fill_manual("",values = Grays)
								
								
								
					# Extract legend from the legend_plot
					tmp <- ggplot_gtable(ggplot_build(legend_plot))
					leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
					mylegend <- tmp$grobs[[leg]]


					# add in legend to the right
					plot_list[[x]] = grid.arrange(arrangeGrob(Raw_plot),mylegend,ncol=2,widths=c(9, 1),newpage=TRUE)
					
				}
			}

			for(i in 1:length(plot_list)){
				assign(paste0("plot_",i),value = plot_list[[i]])
			}


			# illustrate how it looks
			#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,
			#	  plot_9,plot_10,nrow = 5,ncol = 2)


			#plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,nrow = 5,ncol = 2)
			n_rows = ceiling(tophitsamount/2)
			n_cols = 2
			g_plot = plot_grid(plotlist = plot_list,nrow = n_rows,ncol = n_cols)



			#g_plot = plot_grid(plotlist = plot_list,ncol = 1)




			if(s_Saving){
				Shapname = ifelse(s_enable_shap_order,"_SHAP_ORDER_V2","")
				plotname = paste0(temp_Outcome,"_",o_subset,"_",p_algo,Shapname,"_Group_differences.png")
				png(paste0(s_TargerDir,plotname),width = s_baseValue*s_Aspect_ratio[1]*n_cols,height = s_baseValue*s_Aspect_ratio[2]*length(plot_list), res = 350)
					plot(g_plot)
				dev.off()          
			}
			
			if(s_Saving){
				Shapname = ifelse(s_enable_shap_order,"_SHAP_ORDER_V2","")
				plotname = paste0(temp_Outcome,"_",o_subset,"_",p_algo,Shapname,"_Group_differences.pdf")
				pdf(paste0(s_TargerDir,plotname),width = 0.6*s_Aspect_ratio[1]*n_cols,height = 0.6*s_Aspect_ratio[2]*length(plot_list))#, #res = 350)
					plot(g_plot)
				dev.off()          
			}

			# if ratio: 			10:2		16:9
			# width per image  = 	2000 		2000
			# heigth per image  = 	400			1125



						
		}

	}


}