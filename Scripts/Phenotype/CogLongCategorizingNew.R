library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(lcmm)
library(cowplot)
setwd("C:/Users/jh1159/Dropbox/PPMI")

# Demographic information/ Curated Baseline Files
Baseline <- read.csv(file = "Curated_Data_Cuts/PPMI_Baseline_Data_02Jul2018.csv", header = TRUE) # Baseline Cut file
controls <- Baseline[which(Baseline$APPRDX == 2), "PATNO"] #196 cases
pd <-  Baseline[which(Baseline$APPRDX == 1), "PATNO"] #423 cases
all <-  Baseline[which(Baseline$APPRDX %in% c(1,2)), "PATNO"] #423 cases
timedf <- data.frame()
EVENT_ID <- c("SC","V01","V03","V04","V05","V06","V08","V10","V12","V13", "V14","V15", "V16")
YearsRough <- c(0.00, 0.25, 0.75, 1.00, 1.50, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00, 9.00)
timedf <- data.frame(EVENT_ID,YearsRough)
updrs <-  read.csv("Motor___MDS-UPDRS/MDS_UPDRS_Part_I.csv", header = T, stringsAsFactors = F) #[X]
updrs <- updrs[which(updrs$EVENT_ID == "BL" & updrs$PATNO %in% pd),]
bltime <- updrs[,c("PATNO","INFODT")]
rm(updrs)
bltime$INFODT <- paste("01",bltime$INFODT, sep = "/")
bltime$INFODT <- as.Date(bltime$INFODT, format = "%d/%m/%Y")
bltime$FirstDate <- bltime$INFODT #First Date annotated for each case for continuous time variable calculation lated
bltime$FirstDate <- add_with_rollback(bltime$FirstDate, days(45))

# Subset PD cases and Clinical cognitive measures
CogClin <- read.csv(file = "Non-motor_Assessments/Cognitive_Categorization.csv", header = T)
CogClin  <- CogClin[which(CogClin$PATNO %in% pd),]
CogClin <- left_join(CogClin,bltime[,c("PATNO","FirstDate")], by = "PATNO")
CogClin$INFODT <- paste("01",CogClin$INFODT, sep = "/")
CogClin$INFODT <- as.Date(CogClin$INFODT, format = "%d/%m/%Y") 
CogClin$ConTime <- time_length(difftime(CogClin$INFODT, CogClin$FirstDate), "years")
CogClin$rConTime <- round(CogClin$ConTime)
CogClin <- CogClin %>% arrange(PATNO,ConTime)


#Force one of the values to a usable time 
CogClin[which(CogClin$PATNO == "3392" & CogClin$EVENT_ID	== "V10"),"rConTime"] <- 4
CogClin$PATNO_Time <- paste(CogClin$PATNO,CogClin$rConTime, sep = "_")

CogClin <- CogClin[-which(CogClin$COGCAT_TEXT == ""),]
CogClin <-  CogClin[-which(duplicated(CogClin$PATNO_Time)),]
CogClin <- CogClin[-which(CogClin$COGCAT_TEXT == "Indeterminate"),]
ggplot(CogClin, aes(x = as.factor(EVENT_ID), y = rConTime))+
  geom_jitter()

# # Join time matrix to annotate Years from baseline
# CogClin <- left_join(CogClin,timedf, by = "EVENT_ID")
# CogClin <- CogClin %>% arrange(PATNO, YearsRough)
# CogClin <- CogClin[-which(is.na(CogClin$YearsRough)),]
# CogClin <- CogClin[-which(is.na(CogClin$COGCAT)),] #Remove those missing categorization


#Annotate the number of repeats per case
Repeats <- CogClin %>% group_by(PATNO) %>% count(PAG_NAME)

#Force an observation at 1.5 years back to year 1 and remove all non yearly observation
#Plots and tables to summarise observations
ggplot(data = CogClin, aes(x = rConTime))+
  geom_histogram(color = "grey", fill = "white",size = 1)+
  theme_cowplot(24)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9))+
  stat_bin(aes(y=..count.. + 2, label=..count..), geom="text",position = position_stack(vjust = 1.1), binwidth=1,size = 6)+
  xlab("Years from Baseline")


table(CogClin$rConTime)

# 200 cases have observations at year 7. But this may exclude cases missing observations at 
# year 7 but present at 6 + 8
#Generate dataframe with missingness
PD_PATNO_EVENT <- c()
for (i in pd) {
  PATNO_EVENT <- paste(i, c(0,1,2,3,4,5,6,7,8,9), sep = "_")
  PD_PATNO_EVENT <- append(PD_PATNO_EVENT, values = PATNO_EVENT)
}
PD_PATNO_EVENT <- data.frame(PD_PATNO_EVENT)
colnames(PD_PATNO_EVENT) <- "PATNO_EVENT"
CogClin$PATNO_EVENT <- CogClin$PATNO_Time
CogClin <- left_join(PD_PATNO_EVENT,CogClin, by = "PATNO_EVENT")
CogClin$PATNO <- as.numeric(str_sub(CogClin$PATNO_EVENT,end = 4))
CogClin$n <- 1
CogClin$YearsRough <- as.numeric(str_sub(CogClin$PATNO_EVENT,start = -1))


# Create wide data frame
COGwide <- CogClin[,c("PATNO","YearsRough","COGCAT_TEXT")] %>% spread(YearsRough,COGCAT_TEXT)
COGwide <- data.frame(COGwide)



#Subset dementia cases
demVect <- c()
demCOG <- data.frame(matrix( ncol = length(colnames(COGwide))))
colnames(demCOG) <- colnames(COGwide)
for (x in 1:nrow(COGwide)){
  if(isTRUE(any(COGwide[x,] == "Dementia"))){
    demVect <- COGwide[x,]
    demCOG <- rbind(demCOG, demVect)
  }
}
demCOG <- demCOG[-1,]
demCOG$CogDecon <- "Dementia"
## SUMSTATS: 45 dementia cases by year 9

MCIVect <- c()
MCICOG <- data.frame(matrix( ncol = length(colnames(COGwide))))
colnames(MCICOG) <- colnames(COGwide)
for (x in 1:nrow(COGwide)){
  if(isTRUE(any(COGwide[x,] == "MCI"))){
    MCIVect <- COGwide[x,]
    MCICOG <- rbind(MCICOG, MCIVect)
  }
}
MCICOG <- MCICOG[-1,]
MCICOG <- MCICOG[-which(MCICOG$PATNO %in% demCOG$PATNO),]
MCICOG$CogDecon <- "MCI"
## SUMSTATS: 56 MCI cases by year 9


COGwide <- COGwide[,c(0:10)]


# Subset cases with observations == 7 years
COGwide <- COGwide[-which(is.na(COGwide$X7)),]
COGwide <- COGwide[-which(COGwide$PATNO %in% demCOG$PATNO),]
COGwide <- COGwide[-which(COGwide$PATNO %in% MCICOG$PATNO),]
## SUMSTATS: 153 cases with observations at year 7

# The below for loops take the COGwide dataframe and annotate overall cognitive trajectories
for(x in 1:ncol(COGwide)){
  COGwide[,x] <- as.character(COGwide[,x])
}

CogDecon <-  c()
Summary <- c()
sumDF <- data.frame(matrix(ncol = 4))
for(x in 1:nrow(COGwide)){
  
  y <- unique(as.character(COGwide[x,c(2:10)]))
  y <- y[complete.cases(y)]
  
  if(isTRUE(y == "Normal")){
    CogDecon <- append(CogDecon,"Normal")
  } else if(any(y == "MCI")){
    CogDecon <- append(CogDecon,"MCI")
  } else if(!any(y == "Normal")){
    CogDecon <- append(CogDecon,"Unstable")
  } else {
    CogDecon <- append(CogDecon,"UnstableNormal")
    p <- COGwide[x,"PATNO"]
    cc <- length(which(COGwide[x,] == "Cognitive Complaint"))
    i <- length(which(COGwide[x,] == "Indeterminate"))
    n <- length(which(COGwide[x,] == "Normal"))
    sumDF <- rbind(sumDF, c(p,cc,i,n))
  }
}

sumDF <- sumDF[-1,]

colnames(sumDF) <- c("PATNO","CognitiveComplaint","Indeterminate","Normal")

table(CogDecon)

## SUMSTATS: 76 Normal, 80 SCD

COGwide$CogDecon  <- CogDecon
COGwide <- rbind(COGwide,demCOG[,c(1:10,12)])
COGwide <- rbind(COGwide,MCICOG[,c(1:10,12)])


#For Dementia cases annotate years to dementia development
firstDem <- c()

for(x in demCOG$PATNO){
  firstDem <- append(firstDem,(CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "Dementia"),"YearsRough"][1]))
}
firstDem <- cbind(demCOG,firstDem)

#For Dementia cases annotate any reverters

reverters <- c()
for(x in demCOG$PATNO){
  lastDem <- CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "Dementia"),"YearsRough"]
  lastDem <- lastDem[length(lastDem)]
  lastNorm <- CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "Normal"),"YearsRough"]
  lastNorm <- lastNorm[length(lastNorm)]
  if(isTRUE(lastDem < lastNorm)){reverters <- append(reverters,x)}
}

## 1 Dementia -> Normal reverter

COGwide <- COGwide[-which(COGwide$PATNO %in% reverters),]

firstDem  <- firstDem[-which(firstDem$PATNO %in% reverters),]
firstDem <- firstDem[,c("PATNO","firstDem")]
colnames(firstDem)[2] <- "YearsToDementia"
# write.csv(firstDem,file = "Case_Summaries/YearstoDementia.csv")


#For MCI cases annotate years to dementia development
# MCICOG <- COGwide[which(COGwide$CogDecon == "MCI"),]

firstMCI <- c()

for(x in MCICOG$PATNO){
  firstMCI <- append(firstMCI,(CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "MCI"),"YearsRough"][1]))
}
firstMCI <- cbind(MCICOG,firstMCI)

#For MCI cases annotate any reverters

reverters <- c()
for(x in MCICOG$PATNO){
  lastMCI <- CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "MCI"),"YearsRough"]
  lastMCI <- lastMCI[length(lastMCI)]
  lastNorm <- CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "Normal"),"YearsRough"]
  lastNorm <- lastNorm[length(lastNorm)]
  if(isTRUE(lastMCI < lastNorm)){reverters <- append(reverters,x)}
}
##SUMSTATS: 14 cases MCI: Reverters
unclearMCI <- c()
for(x in MCICOG$PATNO){
  startMCI <- CogClin[which(CogClin$PATNO == x & CogClin$COGCAT_TEXT == "MCI"),"YearsRough"][1]
  ClinCheck <- CogClin[which(CogClin$PATNO == x),]
  lastObs <- max(ClinCheck[-which(is.na(ClinCheck$PAG_NAME)),"YearsRough"])
  if(isTRUE(startMCI == lastObs)){unclearMCI <- append(unclearMCI,x)}
}

COGwide <- COGwide[-which(COGwide$PATNO %in% reverters),]

firstMCI  <- firstMCI [-which(firstMCI$PATNO %in% reverters),]
firstMCI <- firstMCI[,c("PATNO","firstMCI")]
colnames(firstMCI)[2] <- "YearsToMCI"

# Fix annotation of 1 late MCI converter 
# CogClin[which(CogClin$COGCAT_TEXT == "MCI" & (CogClin$PATNO %in%  COGwide[which(COGwide$CogDecon %in% c("Normal")),"PATNO"])),]
# COGwide[which(COGwide$PATNO == "3953"),"CogDecon"] <- "MCI"

Repeats$PATNO <- as.character(Repeats$PATNO)

COGwide <- left_join(COGwide,Repeats, by ="PATNO")

################################### Alluvial Plots
library(ggalluvial)
library(cowplot)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
(cols <- f("Dark2"))

CogClin$COGCAT_TEXT <- factor(CogClin$COGCAT_TEXT, levels = c("Missing","Dementia","MCI"
                                                                ,"Cognitive Complaint","Indeterminate","Normal"))

CogClin[which(is.na(CogClin$COGCAT_TEXT)),"COGCAT_TEXT"] <- "Missing" 

values = c("Normal" = cols[1],"Indeterminate" = cols[8],"Cognitive Complaint" = cols[3],
           "MCI" = cols[6],"Dementia" =cols[2], "Missing" = "grey97")


Unstable <- COGwide[which(COGwide$CogDecon == "UnstableNormal"),"PATNO"]
Normal <- COGwide[which(COGwide$CogDecon == "Normal"),"PATNO"]


# for (x in COGwide$PATNO) {
#   if (any(COGwide[which(COGwide$PATNO == x),] == "Indeterminate",na.rm = T) & (COGwide[which(COGwide$PATNO == x),"CogDecon"] %in% c("UnstableNormal","Unstable"))) {
#     COGwide[which(COGwide$PATNO == x),"CogDecon"] <- "Indeterminate"
#   }
# }



COGwide[which(COGwide$CogDecon == "UnstableNormal"),"CogDecon"] <- "SCD"
COGwide[which(COGwide$CogDecon == "Unstable"),"CogDecon"] <- "SCD"
COGwide <- COGwide[-which(COGwide$n < 6 & COGwide$CogDecon %in% c("SCD","Normal")),]
COGwide <- COGwide[-which(COGwide$PATNO %in% firstMCI[which(firstMCI$YearsToMCI == 9),"PATNO"]),]

NewNormal <- COGwide[which(COGwide$CogDecon %in% c("SCD","Normal")),"PATNO"] 
Dementia <- COGwide[which(COGwide$CogDecon %in% c("Dementia")),"PATNO"] 
Ind <- COGwide[which(COGwide$CogDecon %in% c("Indeterminate")),"PATNO"] 
MCI <- COGwide[which(COGwide$CogDecon %in% c("MCI")),"PATNO"]


ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8","9") & 
                       (CogClin$PATNO %in%  c(Dementia,MCI))),],
       aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
           y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(24)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1))+
  scale_fill_manual(values = values)+
  # ylim(0,70)+
  xlab("Years from Baseline")+
  labs(fill = "Cognitive Category")+
  ylab("Number of Subjects")


ggplot(CogClin[-which(CogClin$PATNO %in%  COGwide$PATNO),],
       aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
           y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(24)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1))+
  scale_fill_manual(values = values)+
  # ylim(0,70)+
  xlab("Years from Baseline")+
  labs(fill = "Cognitive Category")+
  ylab("Number of Subjects")


SankeyAll <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8")),],
                    aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
                        y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(20)+
  xlab(NULL)+
  ylab(NULL)+
  # ggtitle("All Subjects")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1, face = "plain"), legend.position = "none")+
  scale_fill_manual(values = values)+
  xlab("Years from Baseline")+
  labs(fill = "Cognitive Category")+
  ylab("Cases (n)")

SankeyLegend <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8") & (CogClin$PATNO %in%  COGwide$PATNO)),],
                    aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
                        y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(26)+
  xlab(NULL)+
  ylab(NULL)+
  # ggtitle("All Subjects")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1), legend.direction = "horizontal",legend.justification = "center")+
  scale_fill_manual(values = values)+
  xlab("Years from Baseline")+
  labs(fill = NULL)+
  ylab("Cases (n)")



SankeyLegend <- get_legend(SankeyLegend)

print(SankeyLegend)

SankeyNormal <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8") & (CogClin$PATNO %in%  COGwide[which(COGwide$CogDecon %in% c("Normal","SCD")),"PATNO"])),],
       aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
           y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(20)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Normal")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1, face = "plain"), legend.position = "none")+
  scale_fill_manual(values = values)+
  xlab(NULL)+
  ylab("Cases (n)")
  

SankeyImpaired <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8") & (CogClin$PATNO %in%  COGwide[which(COGwide$CogDecon %in% c("Dementia","MCI")),"PATNO"])),],
       aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
           y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(20)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Impaired")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1, face = "plain"), legend.position = "none")+
  scale_fill_manual(values = values)

Annotated <- COGwide[which(COGwide$CogDecon %in% c("Normal","MCI","Dementia")),"PATNO"]
Unannotated <- unique(CogClin[-which(CogClin$PATNO %in% Annotated),"PATNO"])


Dementia <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8") & (CogClin$PATNO %in%  COGwide[which(COGwide$CogDecon %in% c("Dementia")),"PATNO"])),],
                   aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
                       y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(20)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Dementia")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1, face = "plain"), legend.position = "none")+
  scale_fill_manual(values = values)+
  xlab(NULL)


Non_Dementia <- ggplot(CogClin[which(CogClin$YearsRough %in% c("1","2","3","4","5","6","7","8") & (CogClin$PATNO %in%  COGwide[which(COGwide$CogDecon %in% c("MCI","SCD","Normal")),"PATNO"])),],
       aes(x = YearsRough, stratum = COGCAT_TEXT, alluvium = PATNO,
           y = n,fill = COGCAT_TEXT, label = COGCAT_TEXT)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 0.8, size = 1) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_cowplot(20)+
  xlab(NULL)+
  ylab("Cases (n)")+
  ggtitle("Non-Dementia")+
  theme(plot.title = element_text(hjust=0.2, vjust = -1, face = "plain"), legend.position = "none")+
  scale_fill_manual(values = values)+
  xlab(NULL)+
  ylab("Cases (n)")


title1 <- ggdraw() + 
  draw_label(
    "Cognitive Impairment Outcome",
    fontface = 'bold',
    x = 0.07,
    hjust = -0.1,
    size = 30
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

title2 <- ggdraw() + 
  draw_label(
    "Dementia Outcome",
    fontface = 'bold',
    x = 0.1,
    hjust = -0.1,
    size = 30
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
title3 <- ggdraw() + 
  draw_label(
    "Years from Baseline",
    x = 0.4,
    hjust = -0.1,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )
title4 <- ggdraw() + 
  draw_label(
    "All Cases",
    fontface = 'bold',
    x = 0.2,
    hjust = -0.1,
    size = 30
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )
title5 <- ggdraw() + 
  draw_label(
    "Cognitive Category",
    x = 0.2,
    hjust = -0.1,
    size = 30,
    fontface = "bold"
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )


ImpairmentPlots <- plot_grid(SankeyNormal,SankeyImpaired, labels = c("B","C"), label_size = 24,align = "h", axis = "b")
DementiaPlots <- plot_grid(Non_Dementia,Dementia, labels = c("D","E"),label_size = 24,align = "h", axis = "b")

Subgroups <- plot_grid(title1,ImpairmentPlots,title2,DementiaPlots,title3,ncol = 1, rel_heights = c(0.2,1,0.2,1,0.1))

SankeySum <- plot_grid(title4,SankeyAll,title5,SankeyLegend,ncol = 1,rel_heights = c(0.1,1,0.1,0.2), labels = c("","A","",""),label_size = 24)


plot_grid(SankeySum,Subgroups,ncol = 2)


tiff(filename = "C:/Users/jh1159/Dropbox/PPMI_Writeup/Publication Manuscript/Submission/Supplemantary Information/Figure S2.tiff",width = 3600, height = 1800,res = 180)
plot_grid(SankeySum,Subgroups,ncol = 2)
dev.off()



BL <- read.csv("Case_Summaries/Baseline_EpigeneticsAdded_140721.csv", header = T)
BL$PATNO <- as.character(BL$PATNO)

BL <- left_join(BL, COGwide[,c("PATNO","CogDecon")], by = "PATNO")

table(BL$Longitudinal_diag,BL$CogDecon)
BL$Longitudinal_diag <- NULL
colnames(BL)[144] <- "Longitudinal_diag"
write.csv(BL, "Case_Summaries/Baseline_EpigeneticsAdded_140721.csv", row.names = F)

png(filename = "C:/Users/jh1159/Dropbox/PPMI_Writeup/Figures/SupplementaryAlluvial.png",width = 2700, height = 1800,res = 210)
Indeteriminate
dev.off()

legend <- get_legend(Indeteriminate + guides(fill=guide_legend(title="Cognitive Diagnosis")))


Alluvial <- plot_grid(SankeyNormal +  theme(legend.position="none"),
          SankeyImpaired +  theme(legend.position="none"),legend, ncol = 3,rel_widths = c(1,1,0.5))


png(filename = "C:/Users/jh1159/Dropbox/PPMI_Writeup/Figures/Alluvial.png",width = 4000, height = 1700,res = 250)
Alluvial
dev.off()

################################## New subsets


BL <- read.csv("Case_Summaries/Baseline_SCD_201021.csv", header = T)
PDD <- read.csv("BinaryPDDlist.csv", header = T)

BL$PATNO <- as.character(BL$PATNO)

BL <- left_join(BL,COGwide[,c("PATNO","CogDecon")], by = "PATNO")

BL$PDD_Group <- NULL
BL$Longitudinal_diag <- NULL
BL[which(BL$CogDecon == "Indeterminate"),"CogDecon"] <- NA

colnames(BL)[143] <- "Longitudinal_diag"

ggplot(BL, aes(x = as.factor(Longitudinal_diag), y = moca))+
  geom_boxplot()+
  geom_jitter()

BL[which(BL$Longitudinal_diag %in% c("SCD","Normal","MCI","Dementia") & BL$moca < 20),]

#write.csv(BL,"Case_Summaries/Baseline_SCD_201021.csv",row.names = F)

BL <- read.csv("Case_Summaries/Baseline_SCD_201021.csv", header = T, stringsAsFactors = F)

################## Summary table
FactorFrame <- read.csv("Data___Databases/FactorLabels_080721.csv",header = T, stringsAsFactors = F)

contdf <- matrix(ncol = 8)
contdf <- data.frame(contdf)
colnames(contdf) <- c("Label","Range","Normal","Impaired","Significance1","NonDementia","Dementia","Significance2")

FactorFrame[which(FactorFrame$Label %in% c("SITE","PATNO","APPRDX","EVENT_ID","hemo_below","hemo_above","updrs1_score")),"Type"] <- "NotAFact"

BL[which(BL$Longitudinal_diag %in% c("SCD","Normal")),"CI"] <- 0
BL[which(BL$Longitudinal_diag %in% c("MCI","Dementia")),"CI"] <- 1
BL[which(BL$Longitudinal_diag %in% c("SCD","Normal","MCI")),"Dem"] <- 0
BL[which(BL$Longitudinal_diag %in% c("Dementia")),"Dem"] <- 1


for(x in 1:nrow(FactorFrame)){
  print(x)
  if(FactorFrame[x,"Type"] == "CON"){
    LAB <- FactorFrame[x,"Label"]
    RNG <- range(BL[,which(colnames(BL) == LAB)],na.rm = TRUE)
    MEANnorm <- mean(BL[which(BL$Longitudinal_diag %in% c("SCD","Normal")),which(colnames(BL) == LAB)],na.rm = TRUE)
    SDnorm <- sd(BL[which(BL$Longitudinal_diag %in% c("SCD","Normal")),which(colnames(BL) == LAB)],na.rm = TRUE)
    MEANimp <- mean(BL[which(BL$Longitudinal_diag %in% c("MCI","Dementia")),which(colnames(BL) == LAB)],na.rm = TRUE)
    SDimp <- sd(BL[which(BL$Longitudinal_diag %in% c("MCI","Dementia")),which(colnames(BL) == LAB)],na.rm = TRUE)
    MEANnondem <- mean(BL[which(BL$Longitudinal_diag %in% c("SCD","Normal","MCI")),which(colnames(BL) == LAB)],na.rm = TRUE)
    SDnondem <- sd(BL[which(BL$Longitudinal_diag %in% c("SCD","Normal","MCI")),which(colnames(BL) == LAB)],na.rm = TRUE)
    MEANdem <- mean(BL[which(BL$Longitudinal_diag == "Dementia"),which(colnames(BL) == LAB)],na.rm = TRUE)
    SDdem <- sd(BL[which(BL$Longitudinal_diag == "Dementia"),which(colnames(BL) == LAB)],na.rm = TRUE)
    
    Ttest <- wilcox.test(BL[which(BL$Longitudinal_diag %in% c("SCD","Normal")),which(colnames(BL) == LAB)],
                    BL[which(BL$Longitudinal_diag %in% c("MCI","Dementia")),which(colnames(BL) == LAB)])$p.value
    Ttest2 <- wilcox.test(BL[which(BL$Longitudinal_diag %in% c("MCI","SCD","Normal")),which(colnames(BL) == LAB)],
                    BL[which(BL$Longitudinal_diag %in% c("Dementia")),which(colnames(BL) == LAB)])$p.value
    
    rawvect <- signif(c(RNG,MEANnorm,SDnorm,MEANimp,SDimp,MEANnondem,SDnondem,MEANdem,SDdem
                        ),3)
    sumvect <-  c(as.character(LAB),paste(rawvect[1],rawvect[2],sep = " to "),
                  paste(rawvect[3],"(",rawvect[4],")",sep = ""),
                  paste(rawvect[5],"(",rawvect[6],")",sep = ""),
                  signif(Ttest,3),
                  paste(rawvect[7],"(",rawvect[8],")",sep = ""),
                  paste(rawvect[9],"(",rawvect[10],")",sep = ""),signif(Ttest2,3))
    
    contdf <- rbind(contdf,sumvect)
    
  }else if(FactorFrame[x,"Type"] %in% c("CAT","ORD")){
    LAB <- FactorFrame[x,"Label"]
    
    TAB <- table(BL$CI, BL[,which(colnames(BL) == LAB)],useNA = "ifany")
    TAB2 <- table(BL$Dem, BL[,which(colnames(BL) == LAB)],useNA = "ifany")
    NAMES <- paste(colnames(TAB),collapse = "/")
    NORMAL <- paste(TAB[which(rownames(TAB) == 0),], collapse = "/")
    IMP <- paste(TAB[which(rownames(TAB) == 1),], collapse = "/")
    
    N_DEM <- paste(TAB2[which(rownames(TAB2) == 0),], collapse = "/")
    DEM <- paste(TAB2[which(rownames(TAB2) == 1),], collapse = "/")
    SIG_1 <- signif(chisq.test(BL[which(BL$CI %in% c(0,1)),"CI"],
                      BL[which(BL$CI %in% c(0,1)),which(colnames(BL) == LAB)])$p.value,3)
    SIG_2 <- signif(chisq.test(BL[which(BL$Dem %in% c(0,1)),"Dem"],
                               BL[which(BL$Dem %in% c(0,1)),which(colnames(BL) == LAB)])$p.value,3)
    sumvect <-  c(as.character(LAB),NAMES,NORMAL,IMP,SIG_1,N_DEM,DEM,SIG_2)
    contdf <- rbind(contdf,sumvect)
  }
}

sumvect
contdf <- contdf[-1,]

LEVS

FactorFrame[which(FactorFrame$Label == "updrs1_score"),]
colnames(contdf)

table(COGwide$CogDecon)
colnames(contdf)
SummaryDF <- left_join(contdf,FactorFrame[,c("Label","Full_Label")], by = "Label")

write.csv(SummaryDF,"_Subject_Characteristics/Summary_Stats_Table.csv")


dataraw_pheno <- read.csv("Case_Summaries/Baseline_EpigeneticsAdded_140721.csv", header = T)

BL <- BL[which(BL$APPRDX == 1),]

table(BL$White, BL$Other)


Remove_features = c("scopa","updrs_totscore","updrs1_score","agediag","rem","ess_cat","TD","gds_cat","td_pigd_old","White","PIGD","rem_cat","BioAge4HAStatic","DNAmAgeSkinBloodClock","DNAmGrimAge")

PDD <- read.csv("BinaryPDDlist.csv", header = T)

table(PDD$BinaryPDD)

s_Lancet_classes <- as.numeric(PDD$BinaryPDD) 
s_Lancet_classes <- -(s_Lancet_classes - 2)

if(s_Lancet_classes==0){
  dataraw_pheno$PDD_Group = NA
  dataraw_pheno$PDD_Group[which(dataraw_pheno$Longitudinal_diag == "Normal")] <- 0#0
  dataraw_pheno$PDD_Group[which(dataraw_pheno$Longitudinal_diag == "MCI")] <- 1#1
  dataraw_pheno$PDD_Group[which(dataraw_pheno$Longitudinal_diag == "Dementia")] <- 1#2
  
  dataraw_pheno$PDD_Group = as.numeric(dataraw_pheno$PDD_Group)
}
if(s_Lancet_classes==1){
  dataraw_pheno$PDD_Group = dataraw_pheno$Lancet_Class
  dataraw_pheno$PDD_Group[which(dataraw_pheno$PDD_Group == "NonDementia")] <- 0#0
  
  dataraw_pheno$PDD_Group[which(dataraw_pheno$PDD_Group == "Dementia")] <- 1#0
  
  dataraw_pheno$PDD_Group = as.numeric(dataraw_pheno$PDD_Group)
  
}


sumv
contdf <- matrix(ncol = 6)

BL <- BL[-which(is.na(BL$Longitudinal_diag)),]


x <- 51
for(x in 1:nrow(FactorFrame)){
  print(x)
  if(FactorFrame[x,"Type"] == "CON"){
    LAB <- FactorFrame[x,"Label"]
    maleMEAN <- mean(BL[which(BL$gen == 1),which(colnames(BL) == LAB)],na.rm = TRUE)
    maleSD <- sd(BL[which(BL$gen == 1),which(colnames(BL) == LAB)],na.rm = TRUE)
    femaleMEAN <- mean(BL[which(BL$gen == 2),which(colnames(BL) == LAB)],na.rm = TRUE)
    femaleSD <- sd(BL[which(BL$gen == 2),which(colnames(BL) == LAB)],na.rm = TRUE)
    
    Ttest <- wilcox.test(BL[which(BL$gen == 1),which(colnames(BL) == LAB)],
                         BL[which(BL$gen == 2),which(colnames(BL) == LAB)])$p.value
    
    rawvect <- signif(c(maleMEAN,maleSD,femaleMEAN,femaleSD,Ttest
    ),3)
    sumvect <-  c(as.character(LAB),rawvect[1],rawvect[2],rawvect[3],rawvect[4],rawvect[5])
    
    contdf <- rbind(contdf,sumvect)
    
  }else if(FactorFrame[x,"Type"] %in% c("CAT","ORD")){
    LAB <- FactorFrame[x,"Label"]
    SIG_1 <- signif(chisq.test(BL$gen,
                               BL[,which(colnames(BL) == LAB)])$p.value,3)
    sumvect <-  c(as.character(LAB),NA,NA,NA,NA,SIG_1)
    contdf <- rbind(contdf,sumvect)
  }
}

contdf
contdf <- contdf[-1,]

bonfP <- 0.05 / nrow(contdf)
contdf <- as.data.frame(contdf)
contdf$V6 <- as.numeric(as.character(contdf$V6))
contdf[which(contdf[,6] < bonfP),]
