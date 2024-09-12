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
################## BUILD THRESHOLD MATRIX ######################################
# load in mix LRs, this is when POI+ (included in mixture)
in_mix_matrix = readRDS("Direct_in_mix_3dresults_051924.RDS")
# load in the informed consent AEHs and pop names 
informed_consent = read_csv("Informed_Consent_AEHs.csv")
# this orders the list alphabetically 
informed_consent = informed_consent[order(informed_consent$...1),]
# represents different LR thresholds to calculate false negative rate (FNR), log LRs are being used
## to calculate the FPR -- "0" = 1, "2" = 100, "4" = 10k, 6" = 1M  
thresholds = c(c(0,2,4,6))
# for loop creates a new matrix of FNRs under each threshold 
for(threshold in thresholds) { 
  # create a new matrix for each threshold, 83 = num of groups, 5 = num of contribs per mixture
  empty_matrix = matrix(nrow = 83, ncol = 5)
  # use a for loop to iterate through the empty matrix
  for (i in 1:dim(empty_matrix)[1]) { # iterates through rows/pops
    for (j in 1:dim(empty_matrix)[2]) { # iterates through LRs/col 
      # calculates the false negative rate per threshold
      false_negative =  sum((in_mix_matrix[,j,i] < threshold))/100000 
      # assigns FNR values to each group and num of contribs in empty matrix 
      empty_matrix[i,j] = false_negative
    }
  }
  # names the matrix for each new matrix created and saves it to the global environment
  assign(paste0("fn_matrix_", threshold), empty_matrix, envir = .GlobalEnv)
}

# Convert each matrix into a data frame 
fn_df_0 = as.data.frame(fn_matrix_0)
fn_df_2 = as.data.frame(fn_matrix_2)
fn_df_4 = as.data.frame(fn_matrix_4)
fn_df_6 = as.data.frame(fn_matrix_6)
# assign the column names to each dataframe 
colnames(fn_df_0) = c(c(2,3,4,5,6))
colnames(fn_df_2) = c(c(2,3,4,5,6))
colnames(fn_df_4) = c(c(2,3,4,5,6))
colnames(fn_df_6) = c(c(2,3,4,5,6))
# label the row names 
rownames(fn_df_0) <- informed_consent$`Average Expected Heterozygosity`
rownames(fn_df_2) <- informed_consent$`Average Expected Heterozygosity`
rownames(fn_df_4) <- informed_consent$`Average Expected Heterozygosity`
rownames(fn_df_6) <- informed_consent$`Average Expected Heterozygosity`
# Transpose the data frame -- this flips rows and columns
fn_df_0 <- as.data.frame(t(fn_df_0))
fn_df_2 <- as.data.frame(t(fn_df_2))
fn_df_4 <- as.data.frame(t(fn_df_4))
fn_df_6 <- as.data.frame(t(fn_df_6))
# assigns a key name to call contribs
fn_df_0$contribs <- rownames(fn_df_0) 
fn_df_2$contribs <- rownames(fn_df_2)
fn_df_4$contribs <- rownames(fn_df_4)
fn_df_6$contribs <- rownames(fn_df_6)
# restructures the df so that contribs column can be called using key word "contribs"
fn_df_0 <- melt(fn_df_0, id.vars='contribs')
fn_df_2 <- melt(fn_df_2, id.vars = 'contribs')
fn_df_4 <- melt(fn_df_4, id.vars = 'contribs')
fn_df_6 <- melt(fn_df_6, id.vars = 'contribs')
################ BUILD QUANTILE POPS FOR GRAPHING ##################################
### These 8 groups show the range of diversity among all 83 groups
fn_df_0quantile_groups <-{ # quantile pops for when threshold is 0
  # tojolabal.240 AEH = 0.6735294
  fn_0_im_group_1 <- fn_matrix_0[77,] 
  # Cree.243 AEH = 0.7411382  
  fn_0_im_group_2 <- fn_matrix_0[25,] 
  # Berber.3 AEH = 0.7728037
  fn_0_im_group_3 <- fn_matrix_0[7,] 
  # Colombia.1_new AEH = 0.7792598
  fn_0_im_group_4 <- fn_matrix_0[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fn_0_im_group_5 <- fn_matrix_0[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fn_0_im_group_6 <- fn_matrix_0[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fn_0_im_group_7 <- fn_matrix_0[4,] 
  # SAColoured.242 AEH = 0.8039703
  fn_0_im_group_8 <- fn_matrix_0[65,] 
  
  # Combine into one data frame 
  fn_0_dataEightCombined <- rbind(fn_0_im_group_1, fn_0_im_group_2, fn_0_im_group_3, 
                                  fn_0_im_group_4, fn_0_im_group_5, fn_0_im_group_6, fn_0_im_group_7, 
                                  fn_0_im_group_8)
  # Assign column names 
  colnames(fn_0_dataEightCombined) <- c(c(2,3,4,5,6)) 
  # assign row names from informed consent data file
  rownames(fn_0_dataEightCombined) <- c(informed_consent$`Average Expected Heterozygosity`[c(1,14,27,40,53,66,79,83)])  
  # flips rows and columns while maintaining data frame structure 
  fn_0_dataEightCombined_df <- as.data.frame(t(fn_0_dataEightCombined)) 
  # adds a new column to the combined data frame called "contribs", wil tell us FPR for a mixture with x num of contribs 
  fn_0_dataEightCombined_df$contribs <- colnames(fn_0_dataEightCombined) 
  # assigns key name "contribs" to be used for graphing purposes 
  fn_0_dataEightCombined_df<- melt(fn_0_dataEightCombined_df, id.vars='contribs') 
  
}
# quantile pops when threshold is 2 
fn_df_2quantile_groups <-{ 
  # tojolabal.240 AEH = 0.6735294
  fn_2_im_group_1 <- fn_matrix_2[77,] 
  # Cree.243 AEH = 0.7411382  
  fn_2_im_group_2 <- fn_matrix_2[25,] 
  # Berber.3 AEH = 0.7728037
  fn_2_im_group_3 <- fn_matrix_2[7,] 
  # Colombia.1_new AEH = 0.7792598
  fn_2_im_group_4 <- fn_matrix_2[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fn_2_im_group_5 <- fn_matrix_2[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fn_2_im_group_6 <- fn_matrix_2[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fn_2_im_group_7 <- fn_matrix_2[4,] 
  # SAColoured.242 AEH = 0.8039703
  fn_2_im_group_8 <- fn_matrix_2[65,] 
  
  # Combine into one data frame 
  fn_2_dataEightCombined <- rbind(fn_2_im_group_1, fn_2_im_group_2, fn_2_im_group_3, 
                                  fn_2_im_group_4, fn_2_im_group_5, fn_2_im_group_6, fn_2_im_group_7, 
                                  fn_2_im_group_8)
  # Assign column names 
  colnames(fn_2_dataEightCombined) <- c(c(2,3,4,5,6)) 
  # Assign row names using informed consent file 
  rownames(fn_2_dataEightCombined) <- c(informed_consent$`Average Expected Heterozygosity`[c(1,14,27,40,53,66,79,83)])  
  # flips rows and columns while maintaining dataframe structure 
  fn_2_dataEightCombined_df <- as.data.frame(t(fn_2_dataEightCombined)) 
  # adds a new column "contribs" which specifies the num of contribs in the mixture 
  fn_2_dataEightCombined_df$contribs <- colnames(fn_2_dataEightCombined) 
  # assigns the key name "contribs" to the num of contributors in a mixture for graphing purposes 
  fn_2_dataEightCombined_df<- melt(fn_2_dataEightCombined_df, id.vars='contribs') 
  
}
# quantile pops when threshold is 4 
fn_df_4quantile_groups <-{
  # tojolabal.240 AEH = 0.6735294
  fn_4_im_group_1 <- fn_matrix_4[77,] 
  # Cree.243 AEH = 0.7411382  
  fn_4_im_group_2 <- fn_matrix_4[25,] 
  # Berber.3 AEH = 0.7728037
  fn_4_im_group_3 <- fn_matrix_4[7,] 
  # Colombia.1_new AEH = 0.7792598
  fn_4_im_group_4 <- fn_matrix_4[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fn_4_im_group_5 <- fn_matrix_4[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fn_4_im_group_6 <- fn_matrix_4[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fn_4_im_group_7 <- fn_matrix_4[4,] 
  # SAColoured.242 AEH = 0.8039703
  fn_4_im_group_8 <- fn_matrix_4[65,] 
  # Combine into one data frame 
  fn_4_dataEightCombined <- rbind(fn_4_im_group_1, fn_4_im_group_2, fn_4_im_group_3, 
                                  fn_4_im_group_4, fn_4_im_group_5, fn_4_im_group_6, fn_4_im_group_7, 
                                  fn_4_im_group_8)
  # Assign column names 
  colnames(fn_4_dataEightCombined) <- c(c(2,3,4,5,6)) 
  # Assign row names using the informed consent file 
  rownames(fn_4_dataEightCombined) <- c(informed_consent$`Average Expected Heterozygosity`[c(1,14,27,40,53,66,79,83)]) 
  # Flips rows and columns while maintaining the data frame structure
  fn_4_dataEightCombined_df <- as.data.frame(t(fn_4_dataEightCombined)) 
  # Adds a new column "contribs" to specify the num of contribs in a mixture 
  fn_4_dataEightCombined_df$contribs <- colnames(fn_4_dataEightCombined) 
  # assigns key name "contribs" for graphing purposes 
  fn_4_dataEightCombined_df<- melt(fn_4_dataEightCombined_df, id.vars='contribs') # assigns key name 
}
# quantile pops when threshold is 6 
fn_df_6quantile_groups <-{
  # tojolabal.240 AEH = 0.6735294
  fn_6_im_group_1 <- fn_matrix_6[77,] 
  # Cree.243 AEH = 0.7411382  
  fn_6_im_group_2 <- fn_matrix_6[25,] 
  # Berber.3 AEH = 0.7728037
  fn_6_im_group_3 <- fn_matrix_6[7,] 
  # Colombia.1_new AEH = 0.7792598
  fn_6_im_group_4 <- fn_matrix_6[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fn_6_im_group_5 <- fn_matrix_6[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fn_6_im_group_6 <- fn_matrix_6[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fn_6_im_group_7 <- fn_matrix_6[4,] 
  # SAColoured.242 AEH = 0.8039703
  fn_6_im_group_8 <- fn_matrix_6[65,] 
  # Combine into one data frame 
  fn_6_dataEightCombined <- rbind(fn_6_im_group_1, fn_6_im_group_2, fn_6_im_group_3, 
                                  fn_6_im_group_4, fn_6_im_group_5, fn_6_im_group_6, fn_6_im_group_7, 
                                  fn_6_im_group_8)
  # Assigns column names, represents num of contribs in a mixture 
  colnames(fn_6_dataEightCombined) <- c(c(2,3,4,5,6)) 
  # Assigns row names using the informed consent file 
  rownames(fn_6_dataEightCombined) <- c(informed_consent$`Average Expected Heterozygosity`[c(1,14,27,40,53,66,79,83)])
  # Flips rows and columns while maintaining dataframe structure 
  fn_6_dataEightCombined_df <- as.data.frame(t(fn_6_dataEightCombined)) 
  # Adds new column "contribs" which specifies num of contribs in a mixture 
  fn_6_dataEightCombined_df$contribs <- colnames(fn_6_dataEightCombined) 
  # Assigns key name to contribs column for graphing purposes
  fn_6_dataEightCombined_df<- melt(fn_6_dataEightCombined_df, id.vars='contribs') 
}
################# CREATE VIOLIN PLOTS FOR EACH THRESHOLD ##############################
# function to convert scientific notation of axes labels
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
# False Negative Rate Distribution plot when threshold is >1
fn_0_plot <- {
  ggplot() + geom_violin(data = fn_df_0, aes(x= as.numeric(contribs), y = value, 
     fill = contribs, ), scale = "width",alpha = 0.30) +  
    xlab("Number of contributors") + ylab("Power") + 
    theme(legend.position = "none", panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),  panel.background = element_blank(), 
    axis.line = element_line(colour = "black")) + scale_fill_manual(values = c("#BCE4D8",
    "#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) +
    geom_point(data = fn_0_dataEightCombined_df, aes(x=as.numeric(contribs), y = value, 
    color = variable)) + geom_line(data = fn_0_dataEightCombined_df, aes(x=as.numeric(contribs), 
    y = value, color = variable)) + xlab("Number of Contributors") + ylab("Power") + 
    labs(colour = "Genetic Diversity", size = 4) + theme(text = element_text(size = 12), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
    axis.line = element_line(colour = "black"),axis.text = element_text(size = 12), legend.key.size = unit(0.2,"cm"),
    legend.position = "bottom", legend.text = element_text(size =12), legend.title = element_text(size = 12),
    legend.box.background = element_rect(color = "black", size = 1), legend.key = element_rect(fill = "white"), 
    legend.key.height = unit(6, "pt"))  + scale_color_manual(labels = c("0.674", "0.741", "0.773", "0.779", "0.784",
    " 0.787", " 0.796", "0.804"), values = rainbow(8)) + guides(fill = "none") 
}
# view the violin plot, y-scale is linear 
fn_0_plot 
# function to extract legend from plot 
get_only_legend <- function(plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  # Mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend) 
}
# extract legend from FN_0 plot 
legend = get_only_legend(fn_0_plot)
# turns the y-axis into log scale, changes format of y-axis labels, and removes axes titles and legend
fn_0_plot =fn_0_plot+ scale_y_continuous(trans = "log10",labels = scientific_10, breaks = pretty_breaks(n=3))+ 
  labs(y=expression("Power"), x = expression("Number of Contributors")) + 
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())
# view final version of plot 
fn_0_plot

# False Negative Rate Distribution Violin Plot when threshold is >100
fn_2_plot <- {
  ggplot() + geom_violin(data = fn_df_2, aes(x= as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) + theme(legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
    axis.line = element_line(colour = "black"), axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text = element_text(size=12)) + 
    scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) + 
    geom_point(data = fn_2_dataEightCombined_df, aes(x=as.numeric(contribs), y = value, color = variable)) + 
    geom_line(data = fn_2_dataEightCombined_df, aes(x=as.numeric(contribs), y = value,
    color = variable)) + scale_color_manual(values = rainbow(8)) + guides(fill = "none") +
    scale_y_continuous(trans = "log10",  labels = scientific_10)  
}
# plot has log scaled y-axis and no legend 
fn_2_plot

# False Negative Rate Distribution Violin Plot when threshold is >10k
fn_4_plot <- {
  ggplot() + geom_violin(data = fn_df_4, aes(x= as.numeric(contribs), y = value, 
    fill = contribs, ), scale = "width", alpha = 0.30) + theme(legend.position = "none", 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank(),axis.line = element_line(colour = "black"),
    axis.text = element_text(size=12)) + scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8",
    "#BCE4D8","#BCE4D8","#BCE4D8")) + geom_point(data = fn_4_dataEightCombined_df, aes(x=as.numeric(contribs),
    y = value, color = variable)) + geom_line(data = fn_4_dataEightCombined_df, 
    aes(x=as.numeric(contribs), y = value, color = variable)) +
    scale_color_manual(values = rainbow(8)) + guides(fill = "none") + 
    scale_y_continuous(trans = "log10", labels = label_number(accuracy = 0.01))  
}
# View the violin plot, y-axis is log scale, no axes text, no legend 
fn_4_plot 
                              
# False Negative Rate Distribution Violin Plot when Threshold is < 1M
fn_6_plot <- {
  ggplot() + geom_violin(data = fn_df_6, aes(x= as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) + theme(legend.position = "none", 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), axis.title.x = element_blank(), axis.title.y = element_blank(),
  axis.text = element_text(size = 12)) + scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8",
  "#BCE4D8")) + geom_point(data = fn_6_dataEightCombined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  geom_line(data = fn_6_dataEightCombined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  scale_color_manual(values = rainbow(8)) + guides(fill = "none") +
  scale_y_continuous(trans = "log10", labels = label_number(accuracy = 0.01))
}
# plot has y-axis in log scale 
fn_6_plot 
                              
# Combine all FNR plots into one panel plot, 4 rows x 1 column 
fn_threshold_plots <- plot_grid(fn_0_plot,fn_2_plot,fn_4_plot, fn_6_plot, nrow = 4, align = "v") 
# add common y-axis title 
fn_threshold_plots<- grid.arrange(fn_threshold_plots, left = "False Negative Rate") 

###################### LOAD NOT IN MIX DATA, POI- (not included) #############################
# read in RDS file
not_in_mix_matrix <- readRDS("Direct_not_in_mix_3dresults_051724.RDS") 
#turns -INFS into -10000, allows values to be graphed
not_in_mix_matrix[not_in_mix_matrix == -Inf] = -10000 
                              
# this for loop will fill an empty matrix with FPRs of varying thresholds 
for(threshold in thresholds) {
  # create a new matrix for each threshold 
  empty_matrix = matrix(nrow = 83, ncol = 5)
  # use a for loop to iterate through the empty matrix
  for (i in 1:dim(empty_matrix)[1]) { # iterates through rows/pops
    for (j in 1:dim(empty_matrix)[2]) { # iterates through LRs/col 
      # calculate the FPR for each threshold
      FPR= sum((not_in_mix_matrix[,j,i]>threshold))/100000 
      empty_matrix[i,j] = FPR
    }
  }
  assign(paste0("FPR_matrix_", threshold), empty_matrix, envir = .GlobalEnv)
}
# turn matrix into data frame 
FPR_df_0 = as.data.frame(FPR_matrix_0)
FPR_df_2 = as.data.frame(FPR_matrix_2)
FPR_df_4 = as.data.frame(FPR_matrix_4)
FPR_df_6 = as.data.frame(FPR_matrix_6)
# assign column names 
colnames(FPR_df_0) <- c(c(2,3,4,5,6)) 
colnames(FPR_df_2) <- c(c(2,3,4,5,6))
colnames(FPR_df_4) <- c(c(2,3,4,5,6))
colnames(FPR_df_6) <- c(c(2,3,4,5,6))
# assign row names 
rownames(FPR_df_0) = informed_consent$`Average Expected Heterozygosity`
rownames(FPR_df_2) = informed_consent$`Average Expected Heterozygosity`
rownames(FPR_df_4) = informed_consent$`Average Expected Heterozygosity`
rownames(FPR_df_6) = informed_consent$`Average Expected Heterozygosity`
# Transpose the data frame -- # flips rows and columns while maintaining DF structure
FPR_df_0<- as.data.frame(t(FPR_df_0))
FPR_df_2 <- as.data.frame(t(FPR_df_2))
FPR_df_4 <- as.data.frame(t(FPR_df_4))
FPR_df_6 <- as.data.frame(t(FPR_df_6))
# creates a key name to call contribs
FPR_df_0$contribs <- rownames(FPR_df_0)
FPR_df_2$contribs <- rownames(FPR_df_2)
FPR_df_4$contribs <- rownames(FPR_df_4)
FPR_df_6$contribs <- rownames(FPR_df_6)
# restructures df 
FPR_df_0 <- melt(FPR_df_0, id.vars='contribs') 
FPR_df_2 <- melt(FPR_df_2, id.vars = 'contribs')
FPR_df_4 <- melt(FPR_df_4, id.vars = 'contribs')
FPR_df_6 <- melt(FPR_df_6, id.vars = 'contribs')                
###################### BUILD QUANTILE POPs ####################################
#### These 8 groups show the range of diversity among all 83 groups ###########
FPR_0_quantile_pops <- {
  # tojolabal.240 AEH = 0.6735294
  fpr_0_group_1 <- FPR_matrix_0[77,] 
  # Cree.243 AEH = 0.7411382  
  fpr_0_group_2 <- FPR_matrix_0[25,] 
  # Berber.3 AEH = 0.7728037
  fpr_0_group_3 <- FPR_matrix_0[7,] 
  # Colombia.1_new AEH = 0.7792598
  fpr_0_group_4 <- FPR_matrix_0[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fpr_0_group_5 <- FPR_matrix_0[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fpr_0_group_6 <- FPR_matrix_0[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fpr_0_group_7 <- FPR_matrix_0[4,] 
  # SAColoured.242 AEH = 0.8039703
  fpr_0_group_8 <- FPR_matrix_0[65,] 
  
# Combined FPR data frame for quantile pops
  fpr_0_buildEight <- {
    fpr_0_combined <- rbind(fpr_0_group_1,fpr_0_group_2,
    fpr_0_group_3, fpr_0_group_4, fpr_0_group_5, fpr_0_group_6,
    fpr_0_group_7, fpr_0_group_8) 
    # turns array into data frame 
    fpr_0_combined_df <- as.data.frame(fpr_0_combined)
    # assign the row names as the group's AEH 
    rownames(fpr_0_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
    # assign column names to the dataframe 
    colnames(fpr_0_combined_df) <- c(2,3,4,5,6)
    # transpose the dataframe 
    fpr_0_combined_df <- as.data.frame(t(fpr_0_combined_df))
    # adds a new column for num of contribs 
    fpr_0_combined_df$contribs <- rownames(fpr_0_combined_df) 
    # assign key name to num of contribs value 
    fpr_0_combined_df <- melt(fpr_0_combined_df, id.vars = "contribs")
  }
}
# Quantile Pops when threshold is > 100
FPR_2_quantile_pops <- {
    # tojolabal.240 AEH = 0.6735294
    fpr_2_group_1 <- FPR_matrix_2[77,] 
    # Cree.243 AEH = 0.7411382  
    fpr_2_group_2 <- FPR_matrix_2[25,] 
    # Berber.3 AEH = 0.7728037
    fpr_2_group_3 <- FPR_matrix_2[7,] 
    # Colombia.1_new AEH = 0.7792598
    fpr_2_group_4 <- FPR_matrix_2[19,] 
    # Portugal.2_new.csv AEH = 0.7840687
    fpr_2_group_5 <- FPR_matrix_2[57,] 
    # Maldives_new.csv AEH = 0.7873452
    fpr_2_group_6 <- FPR_matrix_2[45,] 
    # Bangladesh.248 AEH = 0.7958121
    fpr_2_group_7 <- FPR_matrix_2[4,] 
    # SAColoured.242 AEH = 0.8039703
    fpr_2_group_8 <- FPR_matrix_2[65,] 
    
    # Combined FPR data frame for quantile pops
    fpr_2_buildEight <- {
      fpr_2_combined <- rbind(fpr_2_group_1,fpr_2_group_2,
                              fpr_2_group_3, fpr_2_group_4, fpr_2_group_5, fpr_2_group_6,
                              fpr_2_group_7, fpr_2_group_8) 
      # turns array into data frame 
      fpr_2_combined_df <- as.data.frame(fpr_2_combined)
      # assign the row names as the group's AEH 
      rownames(fpr_2_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
      # assign column names to the dataframe 
      colnames(fpr_2_combined_df) <- c(2,3,4,5,6)
      # transpose the dataframe 
      fpr_2_combined_df <- as.data.frame(t(fpr_2_combined_df))
      # adds a new column for num of contribs 
      fpr_2_combined_df$contribs <- rownames(fpr_2_combined_df) 
      # assign key name to num of contribs value 
      fpr_2_combined_df <- melt(fpr_2_combined_df, id.vars = "contribs")
    }
}

# FPR Quantile Groups when threshold is > 10K
FPR_4_quantile_pops <- {
  # tojolabal.240 AEH = 0.6735294
  fpr_4_group_1 <- FPR_matrix_4[77,] 
  # Cree.243 AEH = 0.7411382  
  fpr_4_group_2 <- FPR_matrix_4[25,] 
  # Berber.3 AEH = 0.7728037
  fpr_4_group_3 <- FPR_matrix_4[7,] 
  # Colombia.1_new AEH = 0.7792598
  fpr_4_group_4 <- FPR_matrix_4[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fpr_4_group_5 <- FPR_matrix_4[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fpr_4_group_6 <- FPR_matrix_4[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fpr_4_group_7 <- FPR_matrix_4[4,] 
  # SAColoured.242 AEH = 0.8039703
  fpr_4_group_8 <- FPR_matrix_4[65,] 
  
  # Combined FPR data frame for quantile pops
  fpr_4_buildEight <- {
    fpr_4_combined <- rbind(fpr_4_group_1,fpr_4_group_2,
                            fpr_4_group_3, fpr_4_group_4, fpr_4_group_5, fpr_4_group_6,
                            fpr_4_group_7, fpr_4_group_8) 
    # turns array into data frame 
    fpr_4_combined_df <- as.data.frame(fpr_4_combined)
    # assign the row names as the group's AEH 
    rownames(fpr_4_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
    # assign column names to the dataframe 
    colnames(fpr_4_combined_df) <- c(2,3,4,5,6)
    # transpose the dataframe 
    fpr_4_combined_df <- as.data.frame(t(fpr_4_combined_df))
    # adds a new column for num of contribs 
    fpr_4_combined_df$contribs <- rownames(fpr_4_combined_df) 
    # assign key name to num of contribs value 
    fpr_4_combined_df <- melt(fpr_4_combined_df, id.vars = "contribs")
  }
}

# FPR Quantile Pops when Threshold > 1M
FPR_6_quantile_pops <- {
  # tojolabal.240 AEH = 0.6735294
  fpr_6_group_1 <- FPR_matrix_6[77,] 
  # Cree.243 AEH = 0.7411382  
  fpr_6_group_2 <- FPR_matrix_6[25,] 
  # Berber.3 AEH = 0.7728037
  fpr_6_group_3 <- FPR_matrix_6[7,] 
  # Colombia.1_new AEH = 0.7792598
  fpr_6_group_4 <- FPR_matrix_6[19,] 
  # Portugal.2_new.csv AEH = 0.7840687
  fpr_6_group_5 <- FPR_matrix_6[57,] 
  # Maldives_new.csv AEH = 0.7873452
  fpr_6_group_6 <- FPR_matrix_6[45,] 
  # Bangladesh.248 AEH = 0.7958121
  fpr_6_group_7 <- FPR_matrix_6[4,] 
  # SAColoured.242 AEH = 0.8039703
  fpr_6_group_8 <- FPR_matrix_6[65,] 
  
  # Combined FPR data frame for quantile pops
  fpr_6_buildEight <- {
    fpr_6_combined <- rbind(fpr_6_group_1,fpr_6_group_2,
                            fpr_6_group_3, fpr_6_group_4, fpr_6_group_5, fpr_6_group_6,
                            fpr_6_group_7, fpr_6_group_8) 
    # turns array into data frame 
    fpr_6_combined_df <- as.data.frame(fpr_6_combined)
    # assign the row names as the group's AEH 
    rownames(fpr_6_combined_df) <- c(informed_consent$`Average Expected Heterozygosity`[c(77,25,7,19,57,45,4,65)])
    # assign column names to the dataframe 
    colnames(fpr_6_combined_df) <- c(2,3,4,5,6)
    # transpose the dataframe 
    fpr_6_combined_df <- as.data.frame(t(fpr_6_combined_df))
    # adds a new column for num of contribs 
    fpr_6_combined_df$contribs <- rownames(fpr_6_combined_df) 
    # assign key name to num of contribs value 
    fpr_6_combined_df <- melt(fpr_6_combined_df, id.vars = "contribs")
  }
}

################## BUILD VIOLIN PLOTS W/ DIFF THRESHOLDS #######################
# False Positive Rate Distribution Plot when Threshold is > 1
fpr_0_plot <- {
  ggplot() + geom_violin(data = FPR_df_0, aes(x=as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) + theme(legend.position = "none",
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
  plot.background = element_rect("white"), axis.text = element_text(size = 12)) + 
  scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8",
  "#BCE4D8","#BCE4D8","#BCE4D8")) + geom_point(data = fpr_0_combined_df, 
  aes(x=as.numeric(contribs), y = value, color = variable)) + geom_line(data = fpr_0_combined_df, 
  aes(x=as.numeric(contribs), y = value, color = variable)) +
  theme(text = element_text(size = 12), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  axis.line = element_line(colour = "black"),axis.text = element_text(size = 12, colour = "black"), 
  legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())  + 
  scale_color_manual(values = rainbow(8)) + guides(fill = "none") + 
  scale_y_continuous(trans = "log10",labels = scientific_10) 
}
# view the final plot 
fpr_0_plot

# False Positive Rate Distribution Plot when Threshold is > 100
fpr_2_plot <- {
  ggplot() + geom_violin(data = FPR_df_2, aes(x=as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + 
  ylab("False positive rate") +theme(legend.position = "none", panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), plot.background = element_rect("white"),
  axis.text = element_text(size = 12, colour = "black"), axis.title.x = element_blank(),
  axis.title.y = element_blank()) + scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8",
  "#BCE4D8","#BCE4D8","#BCE4D8")) + geom_point(data = fpr_2_combined_df, aes(x=as.numeric(contribs), y = value, color = variable)) + 
  geom_line(data = fpr_2_combined_df, aes(x=as.numeric(contribs), y = value, 
  color = variable))  + scale_color_manual(values = rainbow(8)) + 
  guides(fill = "none")  + scale_y_continuous(trans = "log10",
  labels = scientific_10, breaks = scales::pretty_breaks(n=2))
}
# View the final plot, y-axis is log scaled 
fpr_2_plot 

# False Positive Rate when Threshold is > 10K
fpr_4_plot <- {
  ggplot() + geom_violin(data = FPR_df_4, aes(x=as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + 
  ylab("False positive rate") + theme(legend.position = "none", panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), plot.background = element_rect("white"),
  axis.title.x = element_blank(), axis.title.y = element_blank(),
  axis.text = element_text(size = 12)) + 
  scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) +
  geom_point(data = fpr_4_combined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  geom_line(data = fpr_4_combined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  scale_color_manual(values = rainbow(8)) + guides(fill = "none") +
    scale_y_continuous(trans = "log10", labels = scientific_10)  
}
# makes y-axis log scale, changes y-axis labels, and removes the legend and axes titles
fpr_4_plot

# False Positive Rate Distribution plot when threshold is > 1M
fpr_6_plot <- {
  ggplot() + geom_violin(data = FPR_df_6, aes(x=as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + 
  ylab("False positive rate") + theme(legend.position = "none", panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), plot.background = element_rect("white"),
  axis.text = element_text(size=12), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) +
  geom_point(data = fpr_6_combined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  geom_line(data = fpr_6_combined_df, aes(x=as.numeric(contribs), y = value, color = variable)) +
  scale_color_manual(values = rainbow(8)) + guides(fill = "none") +
  scale_y_continuous(trans = "log10", labels = scientific_10, 
  breaks = scales::pretty_breaks(n=2))
}
# makes the y-axis log scaled, changes y-axis text, removes axes titles and legend
fpr_6_plot
                              
# combine all of the fpr plots into one panel plot with 4 rows and 1 column
fpr_threshold_plots <- plot_grid(fpr_0_plot,fpr_2_plot,fpr_4_plot,fpr_6_plot, 
  nrow =4, ncol = 1, align = "v")
# adds common y-axis title 
fpr_threshold_plots <- grid.arrange(fpr_threshold_plots, left = "False Positve Rate")
# combine fpr and fnr threshold plots into one panel plot w/ 4 rows x 2 columns
combined_threshold_plot <- ggarrange(fpr_threshold_plots, fn_threshold_plots, ncol = 2) 
# add a common x-axis title to the plot
combined_threshold_plot <- annotate_figure(combined_threshold_plot, 
  bottom = text_grob("Number of Contributors"))
# adds a common legend to the final plot
combined_threshold_plot <- grid.arrange(combined_threshold_plot, legend, nrow = 2, heights = c(5,1))# add legend to plot



