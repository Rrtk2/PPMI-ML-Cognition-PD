#-----------------------------------------------------------------------------------------------------#
#							Libraries and Working Directories
#-----------------------------------------------------------------------------------------------------#
library(methylumi) 
library(wateRmelon) 
require(gdata) 
library(minfi) 
library(ggplot2) 
require(gridExtra) 
library(plyr) 
require(IlluminaHumanMethylationEPICmanifest)
library(dplyr)
library(viridis)
setwd("/mnt/data1/Josh/PPMI/Methylation_data/Pipeline/")

# #  
## create a folder to sort QC output
# if(!dir.exists("QC")){
#   dir.create("QC")
# }
# if(!dir.exists("QC/Plots")){
#   dir.create("QC/Plots")
# }
 
#-----------------------------------------------------------------------------------------------------#
#							Input and Aims
#-----------------------------------------------------------------------------------------------------#
# Requirements:
# 1. Raw IDAT Files Sourced and decompressed in folder 
# 2. Pheno File
# 
# Aims: QC Raw IDAT data for the PPMI cohort and generate:
# 1. A QC metrics file of each case
# 2. A matrix of raw Beta values (SNP and cross hybrid probes removed)
# 3. A matrix of raw M values
# 4. A matrix of most variable M values
# 5. A matrix of cell type composition

#-----------------------------------------------------------------------------------------------------#
#							Load Pheno file
#-----------------------------------------------------------------------------------------------------#

#Load sample list from Methylation data
pheno <- read.table(file = "/mnt/data1/Josh/PPMI/Methylation_data/Raw_IDATs/Project120_IDATS_n524final_toLONI_030718/PPMI_Meth_n524_for_LONI030718.txt", header = T)
#Annotated Basename using sentrix ID and Sentrix Position for each sample
pheno <-cbind(paste(pheno$Sentrix_ID, pheno$Sentrix_Position, sep = "_"), pheno)
colnames(pheno)[1] <- "Basename"
pheno$Basename <- as.character(pheno$Basename)

#Annotate Site,diagnosis,demographic variables 
SITE <- read.csv("/mnt/data1/Josh/PPMI/_Subject_Characteristics/Center-Subject_List.csv",header = T)
pheno <- left_join(pheno,SITE, by = "PATNO")
diag <- read.csv("/mnt/data1/Josh/PPMI/_Subject_Characteristics/Patient_Status.csv",header = T)
dem <-  read.csv("/mnt/data1/Josh/PPMI/_Subject_Characteristics/Screening___Demographics.csv", header = T)
pheno <-  left_join(pheno,diag[,c("PATNO","ENROLL_CAT")], by = "PATNO")
pheno <- left_join(pheno,dem[,c("PATNO","GENDER")],by="PATNO")

#Recode GENDER to Binary Sex variable
pheno[which(pheno$GENDER == 2),"sex"] <- 1 
pheno[which(pheno$GENDER == 1 |pheno$GENDER == 0),"sex"] <- 0 
pheno$sex <- as.factor(pheno$sex) 

#Annotate BL Age
BL <- read.csv("/mnt/data1/Josh/PPMI/Curated_Data_Cuts/PPMI_Baseline_Data_02Jul2018.csv",header = T)
pheno <-  left_join(pheno,BL[,c("PATNO","age")],by = "PATNO")
 
#-----------------------------------------------------------------------------------------------------#
#							mSet Object Creation
#-----------------------------------------------------------------------------------------------------#

#Annotate idat file path 
 idatPath<-c("/mnt/data1/Josh/PPMI/Methylation_data/Raw_IDATs/Project120_IDATS_n524final_toLONI_030718") ## path to folder where idats are located

#Generate mset object  
#  msetEPIC <- readEPIC(idatPath=idatPath, barcodes = pheno$Basename) ## load data from idats for all samples in phenotype file

#Save mset File 
# save(msetEPIC,file = "mSetPPMI.Rdata") 

#-----------------------------------------------------------------------------------------------------#
#							Intensity Check
#-----------------------------------------------------------------------------------------------------# 

 #extract sample intensities
m_intensities<-methylated(msetEPIC) ## this gives a matrix where each row is a probe and each column a sample
u_intensities<-unmethylated(msetEPIC)

# summarise the intensities of each sample with a single value, the median
M.median<-apply(m_intensities, 2, median)
U.median<-apply(u_intensities, 2, median)

# Summarise median intensity per subject 
QCmetrics<-cbind(pheno, M.median, U.median) ## create a table to store output of QC pipeline
 
# Summarise median intensity per subject 

intens.Thres<-2000 ## change this to adjust the threshold at which you filter
 
#Plot Intensity Distribution per subject
pdf("QC/Plots/Scatterplot_SampleIntensity.pdf")
  par(mfrow = c(1,2))
  hist(M.median, xlab = "Median M intensity")
  hist(U.median, xlab = "Median U intensity")
  par(mfrow = c(1,1))
  plot(M.median, U.median, pch = 16, xlab = "Median M intensity", ylab = "Median U intensity")
  abline(v = intens.Thres, col = "red")
  abline(h = intens.Thres, col = "red")
 dev.off()

# Plots show synstemic low intensity of unmethylated values, Intensity threshold set at 1200 for Methylated, 600 for unMethylated 
 
#Plot intensities colored by possible confounders 
pdf("QC/Plots/IntensityConfounders.pdf",width = 8, height = 6)
 par(1,2)
 plot(M.median, U.median, pch = 16, xlab = "Median M intensity", ylab = "Median U intensity", col = rainbow(nlevels(factor(pheno$Sentrix_ID)))[factor(pheno$Sentrix_ID)], main="Scatter plot of Signal Intensities coloured by Plate") 
 legend("topright", levels(factor(pheno$Sentrix_ID)), col = rainbow(nlevels(factor(pheno$Sentrix_ID))), pch = 10, cex=0.5) 
 abline(v = M.cutoff, col = "red")
 abline(h = U.cutoff, col = "red")
 plot(M.median, U.median, pch = 16, xlab = "Median M intensity", ylab = "Median U intensity", col = rainbow(nlevels(factor(pheno$CNO)))[factor(pheno$CNO)], main="Scatter plot of Signal Intensities coloured by Centre")
 legend("topright", levels(factor(pheno$CNO)), col = rainbow(nlevels(factor(pheno$CNO))), pch = 10, cex=0.5)
 plot(M.median, U.median, pch = 16, xlab = "Median M intensity", ylab = "Median U intensity", col = rainbow(nlevels(pheno$sex))[pheno$sex], main="Scatter plot of Signal Intensities coloured by Sex")
 legend("topright", levels(pheno$sex), col = rainbow(nlevels(pheno$sex)), pch = 10, cex=0.5)
 dev.off()

#Some evidence of chip-specific intensity confounding
 
#calculate a summary statistic for each chip
chip.M.median<-aggregate(M.median, by = list(unlist(strsplit(colnames(m_intensities), "_"))[seq(from = 1, to = 2*ncol(m_intensities), by = 2)]), FUN = median)
chip.U.median<-aggregate(U.median, by = list(unlist(strsplit(colnames(m_intensities), "_"))[seq(from = 1, to = 2*ncol(u_intensities), by = 2)]), FUN = median)
#  
#plot each plate as a boxplot
 pdf("QC/Plots/Boxplot_SampleIntensity_ByPlate.pdf")
 par(mfrow = c(1,2))
 par(mar = c(8, 4, 1, 1))
 nCol<-length(unique(pheno$Sentrix_ID))## assumes there is a column called Plate in your phenotype file
 boxplot(M.median ~ pheno$Sentrix_ID, ylab = "Median M intensity", xlab = "Plate", las = 2, col = rainbow(nCol))
 abline(h=M.cutoff,col="red")
 boxplot(U.median ~ pheno$Sentrix_ID, ylab = "Median U intensity", xlab = "Plate", las = 2, col = rainbow(nCol))
 abline(h=U.cutoff,col="red")
 dev.off()

#Plot difficult to interpret but evidence of chip effect  

#Set Intensity Thresholds and annotate Passed and Failed Subjects
QCmetrics$IntensityCutoff <-  NULL
length(which((U.median > 600) & (M.median > 1200)))
QCmetrics[which((U.median > 600) & (M.median > 1200)),"IntensityCutoff"] <- "Pass" 
QCmetrics[which((U.median < 600) | (M.median < 1200)),"IntensityCutoff"] <- "Fail"

plotFactor <- as.factor(QCmetrics$IntensityCutoff)
plot(M.median, U.median, pch = 16, xlab = "Median M intensity", ylab = "Median U intensity", col = rainbow(nlevels(plotFactor))[plotFactor], main="Scatter plot of Signal Intensities coloured by Cutoff")
legend("topright", levels(QCmetrics$IntensityCutoff), col = rainbow(nlevels(QCmetrics$IntensityCutoff)), pch = 10, cex=0.5)


#14 cases fail intensity cutoff

#-----------------------------------------------------------------------------------------------------#
#							Bisulphite Conversion Test
#-----------------------------------------------------------------------------------------------------# 

#Calculate bisulphite conversion  
bs <- bscon(msetEPIC)

#Plot bisulphite conversion % distribution  
pdf("QC/Plots/HistogramBisulphiteConversionStatistics.pdf")
 hist(bs, xlab = "Median % BS conversion", main = "")
 abline(v = 80, col = "red")
 dev.off()
 
#Add variable for QC metrics  
QCmetrics<-cbind(QCmetrics, bs)
 
#Annotate sample QC failure. A more lenient 70% cutoff applied
 QCmetrics[which(QCmetrics$bs < 70),"BScutoff"] <- "Fail"
 QCmetrics[which(QCmetrics$bs > 70),"BScutoff"] <- "Pass"

#-----------------------------------------------------------------------------------------------------#
#							Gender Check QC
#-----------------------------------------------------------------------------------------------------# 
 
# Generate preliminary beta matrix (This takes a while)
betas <- betas(msetEPIC)

# Match pheno files rows to beta matrix collumn order
pheno<-pheno[match(colnames(betas), pheno$Basename),]
 
#Load EWAS functions (note, also available at ExeterEWASpipeline GitLab)
source("/mnt/data1/ExeterEWASPipeline/R/clusterGender.r") 
source("/mnt/data1/ExeterEWASPipeline/R/findGenderPC.r")

#Generate clustered gender plot  
pdf("QC/Plots/ClusterGenders.pdf")
 predSex1<-findGenderPC(betas, pheno$sex)
 predSex2<-clusterGender(betas, pheno$sex, thres = 0.8)
 dev.off()

#Annotate Sex Checks  
QCmetrics<-cbind(QCmetrics, predSex1, predSex2)

#Annotate Cases With discordant predicted and recorded Sex   
QCmetrics[which(QCmetrics$sex != QCmetrics$predSex1),"SexQC"] <- "Fail"
QCmetrics[which(QCmetrics$sex == QCmetrics$predSex1),"SexQC"] <- "Pass"

#-----------------------------------------------------------------------------------------------------#
#		      SNP test					
#-----------------------------------------------------------------------------------------------------# 

#Extract snp array probes
 betas.rs<-betas[grep("rs", rownames(betas)),]
 
#Correlate probes 
snpCor<-cor(betas.rs, use="complete.obs")
# extract names
names(snpCor)<-pheno$Basename ## the deafult here is the basename of the sample which you may wish to change to a more user friendly identifier
for(i in 1:ncol(betas.rs)){
 	  snpCor[i,i]<-NA
    }

#Calculate max correlation between samples
corMax<-apply(snpCor, 1, max, na.rm = TRUE) ## calculates the maximum correlation for each sample with all other samples (except for itself)

#Distribution of sample correlation
pdf("QC/Plots/SNPCorrelations.pdf")
 hist(corMax, xlab = "Max. correlation with all other samples", main = "")
 dev.off()
  
#Annotate duplicate samples
pdf("QC/Plots/DuplicateSamples.pdf", width = 15, height = 8)
  plot(betas.rs[,which(corMax > 0.95)[1]], betas.rs[,which(corMax > 0.95)[2]], xlab = colnames(betas.rs[,which(corMax > 0.95)[1]]), ylab = colnames(betas.rs[,which(corMax > 0.95)[2]]))
  dev.off()

#Annotate corMax per individual  
QCmetrics <- cbind(QCmetrics,corMax)
 
#Annotate samples with cryptic relatedness
QCmetrics[which(QCmetrics$corMax >0.95),"RelatedQC"] <- "Fail"
QCmetrics[which(QCmetrics$corMax <0.95),"RelatedQC"] <- "Pass"

#2 cases removed for cryptic relatedness

#-----------------------------------------------------------------------------------------------------#
#		      Age prediction				
#-----------------------------------------------------------------------------------------------------# 

#Predict age  
dnamage<-agep(betas)

#Plot Predicted ages
pdf("QC/HistogramOfPredictedAges.pdf")
 hist(dnamage, xlab = "DNAmAge")
 plot(dnamage, pheno$age, xlab = "Predicted", ylab = "Reported")
 dev.off()

#Generate predicted age 
QCmetrics<-cbind(QCmetrics, dnamage)

#Save QC metrics at this point for future use
# write.csv(QCmetrics,file = "QCmetrics_full.csv",row.names = F)

#-----------------------------------------------------------------------------------------------------#
#		      Pfiltering			
#-----------------------------------------------------------------------------------------------------# 

 ##Remove cases missing QCmetrics 
pass <- QCmetrics[which(QCmetrics$IntensityCutoff == "Pass" &
                           QCmetrics$SexQC == "True" &
                           QCmetrics$RelatedQC == "Pass"),
                   "Basename"]  
 
#Remove cases from the mset failing QC
msetEPIC <- msetEPIC[,which(colnames(msetEPIC) %in% QCmetrics$Basename)] 

#Remove cases from QCmetrics missing QC
QCmetrics <- QCmetrics[which(QCmetrics$Basename %in% pass),]

#Reorder QC metrics
QCmetrics <-  QCmetrics[match(colnames(msetEPIC),QCmetrics$Basename),]

#Apply p-filter to mset Object
msetEPIC.pf <- pfilter(msetEPIC)
pFilterPass<-colnames(betas(msetEPIC)) %in% colnames(betas(msetEPIC.pf))

# 1 samples having 1 % of sites with a detection p-value greater than 0.05 were removed 
# Samples removed: 200989060152_R08C01 
# 1025 sites were removed as beadcount <3 in 5 % of samples 
# 10367 sites having 1 % of samples with a detection p-value greater than 0.05 were removed 
 
QCmetrics<-cbind(QCmetrics, pFilterPass)
QCmetrics <- QCmetrics[-which(QCmetrics$pFilterPass == FALSE),]
 
#subset mset probes passing pfiltering 
msetEPIC<-msetEPIC[rownames(betas(msetEPIC)) %in% rownames(betas(msetEPIC.pf)),]

#-----------------------------------------------------------------------------------------------------#
#		      Beta Normalisation			
#-----------------------------------------------------------------------------------------------------# 

#Perform dansensen normalisation
msetEPIC.dasen<-dasen(msetEPIC.pf)


plotmset_density<-function(mset, study=""){ 
  onetwo<-fData(mset)$DESIGN 
  mat<-betas(mset) 
  
  plot(density(mat[onetwo=="I",1], na.rm=T, bw=0.03), cex.main=0.8, main=paste(study, "Betas"), ylim=c(0, 5.2), xlab="") 
  lines(density(mat[onetwo=="II",1], na.rm=T, bw=0.03), col="red") 
  
  for(j in 2:ncol(mat)){ 
    lines(density(mat[onetwo=="I",j], na.rm=T, bw=0.03)) 
    lines(density(mat[onetwo=="II",j], na.rm=T, bw=0.03), col="red") 
  } 
  
  legend("topright", legend=c("Type I", "Type II"), lty=1, col=c("black", "red"))  
} #The more samples in your mset the more messy these plots will look 


RawDistributions <- plotmset_density(msetEPIC.pf, study="Filtered Raw")

#Plot normalised distributions
pdf("NormBetaDist.pdf",width = 10,height = 5)
  plotmset_density(msetEPIC.dasen, study="Filtered Normalised")
  dev.off()

#Check for any odd betas  
raw.weird<-sum(colSums(betas(msetEPIC) > 1 | betas(msetEPIC) <0, na.rm=T) > 0) 
#[1] 0 
dasen.weird<-sum(colSums(betas(msetEPIC.dasen) > 1 | betas(msetEPIC.dasen) < 0) > 0)
#[1] 0                 



#-----------------------------------------------------------------------------------------------------#
#		     RGSet Generation and cell composition calculation		
#-----------------------------------------------------------------------------------------------------# 

#Generate RGSet (This takes a while)
RGSet <- read.metharray.exp(base = idatPath, targets = QCmetrics)
#save(RGSet,"RGSet.Rdata")

#Preprocess
GRset.noob <- preprocessNoob(RGSet)

#Source required base data
library(ExperimentHub)  
hub <- ExperimentHub()  
query(hub, "FlowSorted.Blood.EPIC")  
FlowSorted.Blood.EPIC <- hub[["EH1136"]]

#Cell type estimation
library("estimateCellCounts2")


counts<-estimateCellCounts2(RGSet, compositeCellType = "Blood",   
                                 processMethod = "preprocessNoob",  
                                 probeSelect = "IDOL",  
                                 cellTypes = c("CD8T", "CD4T", "NK", "Bcell",  
                                               "Mono", "Neu"),  
                                 referencePlatform =   
                                   "IlluminaHumanMethylationEPIC",  
                                 referenceset = NULL,  
                                 IDOLOptimizedCpGs =IDOLOptimizedCpGs,   
                                 returnAll = FALSE)  

#save(counts,file = "count.Rdata")

load("mSet_pFil.Rdata")

#Generate final beta matrix
betas <- betas(msetEPIC.dasen)
# save(betas,"finalNormBetas.Rdata")

#-----------------------------------------------------------------------------------------------------#
#		     Redundant Probe removal and M-value generation	
#-----------------------------------------------------------------------------------------------------# 

# Source cross hybridised, snp and sex probes
crosshyb<-read.table("/mnt/data1/EPIC_reference/CrossHydridisingProbes_McCartney.txt", stringsAsFactors = FALSE) 
snpProbes<-read.table("/mnt/data1/EPIC_reference/SNPProbes_McCartney.txt", stringsAsFactors = FALSE, header = TRUE) 
snpProbes<-snpProbes[which(snpProbes$EUR_AF >= 0.05 & snpProbes$EUR_AF <= 0.95),] 
epicManifest <-read.csv("/mnt/data1/EPIC_reference/MethylationEPIC_v-1-0_B4.csv", header = T, stringsAsFactors = F,skip = 7)
rownames(epicManifest)<-epicManifest[,1]
length(which(epicManifest$CHR == "Y" | epicManifest$CHR == "X"))
SexProbes <-  rownames(epicManifest)[which(epicManifest$CHR == "Y" | epicManifest$CHR == "X")]

# Drop cross hyb and SNP probes
betas<-betas[!(rownames(betas) %in% crosshyb[,1]), ] 
betas<-betas[!(rownames(betas) %in% unique(snpProbes$IlmnID)), ] 
betas<-betas[-grep("rs", rownames(betas)),] 

# Drop X and Y probes
betas <- betas[-which(rownames(betas)%in% SexProbes),]

# Load latent class annotated pheno file
BL <- read.csv("/mnt/data1/Josh/PPMI/Collated_Phenotype_Files/BaselinePRS_07102020.csv", header = T)
BL$Basename <- paste(BL$Sentrix.ID, BL$Sentrix.Position, sep = "_")
BL <- BL[-which(is.na(BL$CogClass)),]


# Only retain relevant Beta values with beta vals for 
betas <- betas[,which(colnames(betas) %in% BL$Basename)]

# Convert Betas to M values
M <- beta2m(betas)

#-----------------------------------------------------------------------------------------------------#
#		     Remove sex effect and annotate most variable probes
#-----------------------------------------------------------------------------------------------------# 
library(limma)

# Subset cases for limma 
BL <- BL[which(BL$Basename %in% colnames(M)),]
BL <- BL[order(match(BL$Basename,colnames(M))),]

# Normalise M values
nosexM <- removeBatchEffect(M,batch=as.factor(BL$gen))


#Extract most variable M values
mad <-apply(nosexM,1,stats::mad)
mad <- sort(mad)
most_var <- tail(mad,n = 100000)
Mvar <- nosexM[which(rownames(nosexM) %in% names(most_var)),]

#Save most variable M values, sex effect removed
save(Mvar,file = "/mnt/data1/Josh/PPMI/Methylation_data/nosexMvar.rdata")
