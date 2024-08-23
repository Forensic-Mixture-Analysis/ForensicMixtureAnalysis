########################### SET WORKING DIRECTORY ##############################
setwd('~/OneDrive - San Francisco State University/LTA_LABWORK/244_Pops_Scripts')
########################### LOAD LIBRARIES AND PACKAGES #########################
load_libraries <- {library(ggplot2)
  library(tidyr)
  library(tidyverse)
  library(plotly)
  library(dplyr)
  library(reshape)
  library(abind)
  library(grid)
  library(gridExtra)
  library(patchwork) 
  library(tibble)
  library(RColorBrewer)
  library(cowplot)
  library(reshape2)
  library(ggpubr)
  library(scales) 
}
################################ LOAD NOT IN MIX DATA #############################
# read in RDS file
not_in_mix_matrix <- readRDS("Direct_not_in_mix_3dresults_051724.RDS") 
#turns -INFS into -10000
not_in_mix_matrix[not_in_mix_matrix == -Inf] = -10000 
################################ TURN INTO DATA FRAME #################################
# Creating matrix: x is # of contributors, y is population
# nrows = number of pops used
FPR_matrix <- matrix(nrow=83, ncol = 5) # matrix will hold False Positive Rates
for (i in 1:dim(FPR_matrix)[1]) { # rows
  for (j in 1:dim(FPR_matrix)[2]){ # columns 
    FPR_matrix[i,j] = sum((not_in_mix_matrix[,j,i]>0))/100000 # calculate FPR
  }
}
#FPR_matrix
# load AEH and group names 
informed_consent = read_csv("Informed_Consent_AEHs.csv")
#informed_consentGD = informed_consent[,2]
######### SUBSET DATA TO ONLY INCLUDE INFORMED CONSENT POPULATIONS ################
# turn the FPR matrix into a DF
FPR_DF <- as.data.frame(FPR_matrix) 
# assign column names 
colnames(FPR_DF) <- c(c(2,3,4,5,6)) 
#FPR_DF
# filter the informed consent groups alphabtetically 
informed_consent = informed_consent[order(informed_consent$...1),]
# Assign row names in the FPR_DF
rownames(FPR_DF) = informed_consent$`Average Expected Heterozygosity`
#informed_consent$`Average Expected Heterozygosity`
# Transpose the data frame -- # flips rows and columns while maintaining DF structure
FPR_DF <- as.data.frame(t(FPR_DF)) 
# creates a key name to call contribs
FPR_DF$contribs <- rownames(FPR_DF)
#FPR_DF
# restructures df 
FPR_DF <- melt(FPR_DF, id.vars='contribs') 
############################## BUILD FBI DATA ###########################################\
# Gather FBI data into one array 
FBI_groups <-{
  # FBI_African_American AEH = 0.7899474
  group1 <- FPR_matrix[31,] 
  # FBI_Apache AEH = 0.7071198
  group2 <- FPR_matrix[32,] 
  # FBI_Cauc AEH = 0.7811042
  group3 <- FPR_matrix[33,]
  # FBI_Guam_Filipino AEH = 0.7741539
  group4 <- FPR_matrix[34,]
  # FBI_Navajo AEH = 0.7015840
  group5 <- FPR_matrix[35,]  
  # FBI_SE_Hispanic AEH = 0.7910676
  group6 <- FPR_matrix[36,] 
  # FBI_SW_Hispanic AEH = 0.7692378
  group7 <- FPR_matrix[37,] 
  # FBI_Trinidad AEH = 0.8026450
  group8 <- FPR_matrix[38,] 
  
}

# Combine the FBI Groups into one data frame 
buildEight <- {
  # Combines groups by binding the rows together 
  FBI_groups_combined <- rbind(group1, group2, group3, group4,
                               group5, group6, group7, group8)
  # Assign column names to the FBI groups 
  colnames(FBI_groups_combined) <- c(c(2,3,4,5,6))
  # Assign row names to the combined data frame 
  rownames(FBI_groups_combined) <- c(informed_consent$`Average Expected Heterozygosity`[c(31,32,33,34,35,36,37,38)])
  # turn array into a data frame 
  FBI_groups_combined <- as.data.frame(t(FBI_groups_combined))
  # add a row specifying the num of contribs per mixture 
  FBI_groups_combined$contribs <- rownames(FBI_groups_combined)
  # create an ID name for contribs row for graphing purposes 
  FBI_groups_combined <- melt(FBI_groups_combined, id.vars='contribs')
}
########################### CREATE FBI PLOT #####################################
# pull 8 colors from the color brewers library package 
colors = brewer.pal(8,"Set2") 
# build the FBI plot 
FBI_Plot <- {
  ggplot() + geom_point(data = FBI_groups_combined, aes(x=as.numeric(contribs), 
  y = value, color = variable), size = 3) + geom_line(data = FBI_groups_combined, 
  aes(x=as.numeric(contribs), y = value, color = variable), size = 1.5) +
  xlab("Number of Contributors") + ylab("False Positive Rate") +
  theme(text = element_text(size = 12), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),panel.background = element_blank(),
  axis.line = element_line(colour = "black"), axis.text = element_text(size = 12,
  color = "black"), legend.position = c(0.25,0.8), legend.text = element_text(size =12, 
  color = "black"), legend.box.background = element_rect(fill = "black", color = NA, 
  size = 1), legend.key.height = unit(12, "pt"), legend.title = element_blank(), 
  legend.key.size = unit(2, "cm"))  + guides(fill = "none") + scale_color_manual(labels = c("African American", 
  "Apache", "Caucasian", "Filipino", "Navajo", "SE Hispanic", "SW Hispanic", 
  "Trinidadian"), values= colors)  + scale_y_continuous(trans = "log10")
}
# view final plot 
FBI_Plot # linear plot of the FBI FPRs 


