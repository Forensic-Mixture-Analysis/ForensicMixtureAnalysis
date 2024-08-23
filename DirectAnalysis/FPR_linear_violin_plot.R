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
# read in RDS file
not_in_mix_matrix <- readRDS("Direct_not_in_mix_3dresults_051724.RDS") 
#turns -INFS into -10000
not_in_mix_matrix[not_in_mix_matrix == -Inf] = -10000 
######################## BUILD DATA FRAME #####################################
# Creating matrix: x is # of contributors, y is population
# nrows = number of pops used
FPR_matrix <- matrix(nrow=83, ncol = 5) # matrix will hold False Positive Rates
for (i in 1:dim(FPR_matrix)[1]) { # rows
  for (j in 1:dim(FPR_matrix)[2]){ # columns 
    FPR_matrix[i,j] = sum((not_in_mix_matrix[,j,i]>0))/100000 # calculate FPR
  }
}
#FPR_matrix
# turn the FPR matrix into a DF
FPR_DF <- as.data.frame(FPR_matrix) 
# assign column names 
colnames(FPR_DF) <- c(c(2,3,4,5,6)) 
#FPR_DF
# load in the informed consent file
informed_consent = read_csv("Informed_Consent_AEHs.csv")
# filter the data alphabetically
informed_consent = informed_consent[order(informed_consent$...1),]
# subset to only include group AEH & assign AEH as row names for FPR_DF 
rownames(FPR_DF) = informed_consent$`Average Expected Heterozygosity`
#informed_consent$`Average Expected Heterozygosity`
# flips rows and columns while maintaining DF structure
FPR_DF <- as.data.frame(t(FPR_DF)) 
# creates a key name to call contribs
FPR_DF$contribs <- rownames(FPR_DF)
#FPR_DF
# restructures df 
FPR_DF <- melt(FPR_DF, id.vars='contribs') 
############################## BUILD DATA FOR QUANTILE POPS #####################
# combine the quantile pops into one single array 
quantile_pops <- {
  # tojolabal.240 AEH = 0.6735294
  group_1 <- FPR_matrix[77,] 
  # Cree.243 AEH = 0.7411382  
  group_2 <- FPR_matrix[25,] 
  # Berber.3 AEH = 0.7728037
  group_3 <- FPR_matrix[7,] 
  # Colombia.1_new AEH = 0.7792598
  group_4 <- FPR_matrix[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  group_5 <- FPR_matrix[57,] 
  # Maldives_new.csv AEH = 0.7873452
  group_6 <- FPR_matrix[45,] 
  # Bangladesh.248 AEH = 0.7958121
  group_7 <- FPR_matrix[4,] 
  # SAColoured.242 AEH = 0.8039703
  group_8 <- FPR_matrix[65,] 
}
# Combined FPR data frame for quantile pops
buildEight <- {
  fpr_combined <- rbind(group_1,group_2,
                        group_3, group_4, group_5, group_6,
                        group_7,group_8) 
  # turns array into data frame 
  fpr_combined_df <- as.data.frame(fpr_combined)
  # assign the row names as the group's AEH 
  rownames(fpr_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
  # assign column names to the dataframe 
  colnames(fpr_combined_df) <- c(2,3,4,5,6)
  # transpose the dataframe 
  fpr_combined_df <- as.data.frame(t(fpr_combined_df))
  # adds a new column for num of contribs 
  fpr_combined_df$contribs <- rownames(fpr_combined_df) 
  # assign key name to num of contribs value 
  fpr_combined_df <- melt(fpr_combined_df, id.vars = "contribs")
}
########## BUILD FALSE POSITIVE RATE DISTRIBUTION PLOT #########################
fpr_plot <- {
  ggplot() + geom_violin(data = FPR_DF, aes(x= as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + 
  ylab("False Positive Rate") + theme(legend.position = "none", panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),panel.background = element_blank(), 
  axis.line = element_line(colour = "black")) + scale_fill_manual(values = c("#BCE4D8",
  "#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) + geom_point(data = fpr_combined_df, 
  aes(x=as.numeric(contribs), y = value, color = variable)) + geom_line(data = fpr_combined_df, 
  aes(x=as.numeric(contribs), y = value, color = variable)) + xlab("Number of Contributors") +
  ylab("False Positive Rate") + labs(colour = "Genetic Diversity",size = 1) + 
  theme(text = element_text(size = 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),axis.line = element_line(colour = "black"),
  axis.text = element_text(size = 12), legend.position = c(0.25,0.80),
  legend.text = element_text(size =12), legend.title = element_text(size = 12),
  legend.box.background = element_rect(color = "black", size = 0.6), 
  legend.key = element_rect(fill = "white"), legend.key.height = unit(2, "pt")) + 
  scale_color_manual(labels = c("0.674", "0.741", "0.773", "0.779", "0.784",
  " 0.787", " 0.796", "0.804"), values = rainbow(8)) + guides(fill = "none") 
}
# view final plot 
fpr_plot 
