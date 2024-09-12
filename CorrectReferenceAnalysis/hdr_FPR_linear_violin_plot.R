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
########################## LOAD POI- (not included in mix) DATA #################
# read in RDS file for nhigh drop out rate results 
hdr_matrix <- readRDS("Direct_not_in_mix_3dresults_041224.RDS")
#turns -INFS into -10000
hdr_matrix[hdr_matrix == -Inf] = -10000 
# load in the informed consent file
informed_consent = read_csv("Informed_Consent_AEHs.csv")
# filter the data alphabetically
informed_consent = informed_consent[order(informed_consent$...1),]
######################## BUILD DATA FRAME #####################################
# Creating matrix: x is # of contributors, y is population
# nrows = number of pops used
hdr_FPR_matrix <- matrix(nrow=83, ncol = 5) # matrix will hold False Positive Rates
for (i in 1:dim(hdr_FPR_matrix)[1]) { # rows
  for (j in 1:dim(hdr_FPR_matrix)[2]){ # columns 
    hdr_FPR_matrix[i,j] = sum((hdr_matrix[,j,i]>0))/100000 # calculate FPR
  }
}
#hdr_FPR_matrix
# turn the FPR matrix into a DF
hdr_FPR_DF <- as.data.frame(hdr_FPR_matrix) 
# assign column names 
colnames(hdr_FPR_DF) <- c(c(2,3,4,5,6)) 
#FPR_DF
# subset to only include group AEH & assign AEH as row names for FPR_DF 
rownames(hdr_FPR_DF) = informed_consent$`Average Expected Heterozygosity`
#informed_consent$`Average Expected Heterozygosity`
# flips rows and columns while maintaining DF structure
hdr_FPR_DF <- as.data.frame(t(hdr_FPR_DF)) 
# creates a key name to call contribs
hdr_FPR_DF$contribs <- rownames(hdr_FPR_DF)
#hdr_FPR_DF
# restructures df 
hdr_FPR_DF <- melt(hdr_FPR_DF, id.vars='contribs') 
############################## BUILD DATA FOR QUANTILE POPS #####################
# combine the quantile pops into one single array 
quantile_pops <- {
  # tojolabal.240 AEH = 0.6735294
  group_1 <- hdr_FPR_matrix[77,] 
  # Cree.243 AEH = 0.7411382  
  group_2 <- hdr_FPR_matrix[25,] 
  # Berber.3 AEH = 0.7728037
  group_3 <- hdr_FPR_matrix[7,] 
  # Colombia.1_new AEH = 0.7792598
  group_4 <- hdr_FPR_matrix[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  group_5 <- hdr_FPR_matrix[57,] 
  # Maldives_new.csv AEH = 0.7873452
  group_6 <- hdr_FPR_matrix[45,] 
  # Bangladesh.248 AEH = 0.7958121
  group_7 <- hdr_FPR_matrix[4,] 
  # SAColoured.242 AEH = 0.8039703
  group_8 <- hdr_FPR_matrix[65,] 
}
# Combined FPR data frame for quantile pops
buildEight <- {
  hdr_combined <- rbind(group_1,group_2,
    group_3, group_4, group_5, group_6,
    group_7,group_8) 
  # turns array into data frame 
  hdr_combined_df <- as.data.frame(hdr_combined)
  # assign the row names as the group's AEH 
  rownames(hdr_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
  # assign column names to the dataframe 
  colnames(hdr_combined_df) <- c(2,3,4,5,6)
  # transpose the dataframe 
  hdr_combined_df <- as.data.frame(t(hdr_combined_df))
  # adds a new column for num of contribs 
  hdr_combined_df$contribs <- rownames(hdr_combined_df) 
  # assign key name to num of contribs value 
  hdr_combined_df <- melt(hdr_combined_df, id.vars = "contribs")
}
########## BUILD FALSE POSITIVE RATE DISTRIBUTION PLOT #########################
hdr_plot <- {
  ggplot() + geom_violin(data = hdr_FPR_DF, aes(x= as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + 
  ylab("False Positive Rate") + theme(legend.position = c(0.25,0.80),text = element_text(size = 12),
  axis.line = element_line(colour = "black"), axis.text = element_text(size = 12),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  legend.text = element_text(size =12),legend.title = element_text(size = 12),
  legend.box.background = element_rect(color = "black", size = 0.6),legend.key = element_rect(fill = "white"),
  legend.key.height = unit(2, "pt")) + scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8",
  "#BCE4D8","#BCE4D8","#BCE4D8")) + geom_point(data = hdr_combined_df, aes(x=as.numeric(contribs), 
  y = value, color = variable)) + geom_line(data = hdr_combined_df, aes(x=as.numeric(contribs), 
  y = value, color = variable)) + labs(colour = "Genetic Diversity",size = 1)+ 
  scale_color_manual(labels = c("0.674", "0.741", "0.773", "0.779", "0.784",
  " 0.787", " 0.796", "0.804"), values = rainbow(8)) + guides(fill = "none") 
}
# view final plot 
hdr_plot 
