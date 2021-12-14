#-----------------------------------------------------------------------------------------------------#
#							Generation of P values for undeterminate samples using specific model
#-----------------------------------------------------------------------------------------------------#
# selecting All:cforest and Clinical:rf OWN class

#-----------------------------------------------------------------------------------------------------#
#							Functions
#-----------------------------------------------------------------------------------------------------#
# Mode
 Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#-----------------------------------------------------------------------------------------------------#
#							Settings
#-----------------------------------------------------------------------------------------------------#
i_class=0;o_subset="Clinical";temp_Lancet = ifelse(i_class,"LancetClass","OwnClass");p_algo="rf"
ROC_Rootdir = "C:/DATA STORAGE/Projects/PPMI/Results"

#-----------------------------------------------------------------------------------------------------#
#							Load data
#-----------------------------------------------------------------------------------------------------#
load(paste0(ROC_Rootdir,"/",temp_Lancet,"/",o_subset,"/",p_algo,"_image.RData"))
# object of interest is called "predictions_all_p"

#-----------------------------------------------------------------------------------------------------#
#							PLot
#-----------------------------------------------------------------------------------------------------#

# upsit is top feature in own:clinical:all, should give some meaning to what the model tries to predict, thus a nice idea to use this one to visualize
upsitorder = data_all[match(rownames(predictions_all_p),rownames(data_all)),'upsit']

# get classes and set them in correct order
Classcols = data_all[match(rownames(predictions_all_p),rownames(data_all)),'Class']
Classcols = factor(Classcols,levels = c(NA, "Normal", "UnstableNormal","Unstable","MCI","Dementia"))

# Transform to numeric
Classcols_num = as.numeric(Classcols)
Classcols_num[is.na(Classcols_num)] = 0

# make more direct colors
Classcols_num[Classcols_num==0] = "grey80"
Classcols_num[Classcols_num==1] = "green"
Classcols_num[Classcols_num==2] = "green4"
Classcols_num[Classcols_num==3] = "darkorange"
Classcols_num[Classcols_num==4] = "red"
Classcols_num[Classcols_num==5] = "red4"

# scatterplot UPSIT
plot(upsitorder,predictions_all_p[,2],col=Classcols_num,pch=ifelse(Classcols_num=="grey80",1,19),main="P of developing severe cognitive impairment",xlab="Upsit",ylab="Probability of Class 2 (PDD+MCI)")

# scatterplot CLASS
plot(predictions_all_p[order(Classcols),2],col=Classcols_num[order(Classcols)],pch=ifelse(Classcols_num[order(Classcols)]=="grey80",1,19),main="P of developing severe cognitive impairment",xlab="Class order (index)",ylab="Probability of Class 2 (PDD+MCI)")

# boxplot
boxplot(list(Undeterminate=predictions_all_p[Classcols_num=="grey80",2],
Normal=predictions_all_p[Classcols_num=="green",2],
UnstableNormal=predictions_all_p[Classcols_num=="green4",2],
Unstable=predictions_all_p[Classcols_num=="darkorange",2],
MCI=predictions_all_p[Classcols_num=="red",2],
Dementia=predictions_all_p[Classcols_num=="red4",2]),las=2,col=c("grey80", "green", "green4","darkorange","red","red4"),ylab="Probability of Class 2 (PDD+MCI)")

	

	