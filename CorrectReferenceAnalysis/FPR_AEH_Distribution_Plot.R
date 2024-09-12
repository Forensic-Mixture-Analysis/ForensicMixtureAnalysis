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

# turn the FPR matrix into a DF
FPR_DF <- as.data.frame(FPR_matrix) 
# assign column names 
colnames(FPR_DF) <- c(c(2,3,4,5,6)) 
#FPR_DF
FPR_DF$AEH = informed_consent$`Average Expected Heterozygosity`
############################### CREATE SCATTERPLOT ###################################################
# scatter distribution plot of mixture w/ 2 contributors vs AEH 
AEH_SP1 <- ggplot(data = FPR_DF, aes(x =FPR_DF[,6],  y = FPR_DF[,1])) + geom_point(size = 1) + 
  labs(x = "Genetic Diversity", y = "False positive rate") + stat_smooth(method = "lm",
  formula = y ~ x,geom = "smooth", se= FALSE) + theme(legend.position = "none",
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))  + 
  theme(text = element_text(size = 15)) + theme(axis.title.x = element_blank(), 
  axis.text.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(-0.04, 0, 0, 0), "cm"))
# view the final plot
AEH_SP1  

# scatter distribution plot of mixtures with 3 contribs vs AEH 
AEH_SP2 <- ggplot(data = FPR_DF, aes(x =FPR_DF[,6],  y = FPR_DF[,2])) + geom_point(size = 1) +
  labs(x = "Genetic Diversity",y = "False positive rate") + stat_smooth(method = "lm",
  formula = y ~ x,geom = "smooth", se= FALSE) + theme(legend.position = "none", 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(text = element_text(size = 15))  + theme(axis.title.x = element_blank(), 
  axis.text.x = element_blank(), axis.title.y = element_blank()) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# view the final plot
AEH_SP2

# scatter distribution plot of mixtures w/ 4 contributors vs AEH 
AEH_SP3 <- ggplot(data = FPR_DF, aes(x =FPR_DF[,6],  y = FPR_DF[,3])) + 
  geom_point(size = 1) + labs(x = "Genetic Diversity", y = "False positive 
  rate") + stat_smooth(method = "lm",formula = y ~ x,geom = "smooth", 
  se= FALSE) + theme(legend.position = "none", panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black")) + theme(text = element_text(size = 15)) +  
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
  axis.title.y = element_blank()) + theme(plot.margin = unit(c(0, 0, 0, 0), 
  "cm"))
# view the final plot 
AEH_SP3 

# scatter distribution plot w/ mixtures including 5 contributors vs AEH 
AEH_SP4 <- ggplot(data = FPR_DF, aes(x =FPR_DF[,6],  y = FPR_DF[,4])) + 
  geom_point(size = 1) + labs(x = "Genetic Diversity", y = "False positive rate") +
  stat_smooth(method = "lm",formula = y ~ x,geom = "smooth", se= FALSE) +
  theme(legend.position = "none", panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(colour = "black"))  + 
  theme(text = element_text(size = 15)) +  theme(axis.title.x = element_blank(), 
  axis.text.x = element_blank(), axis.title.y = element_blank()) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# view the final plot 
AEH_SP4 

# scatter distribution plot w/ mixtures including 6 contributors vs AEH 
AEH_SP5 <- ggplot(data = FPR_DF, aes(x =FPR_DF[,6],  y = FPR_DF[,5])) + 
  geom_point(size= 1) + labs(x = "Genetic Diversity") + stat_smooth(method = "lm",
  formula = y ~ x,geom = "smooth", se= FALSE) + theme(legend.position = "none", 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(text = element_text(size = 15)) + labs(y= "False Positive Rate") + 
  theme( axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# view the final plot 
AEH_SP5 

# combine all plots into one panel plot with 1 column and 5 rows 
unified_plot <- plot_grid(AEH_SP1,AEH_SP2,AEH_SP3, AEH_SP4, AEH_SP5, 
                   nrow =5, align = "hv")
# view the unified plot 
unified_plot
# add axes titles to the plot 
grid.arrange(unified_plot,  bottom = textGrob("Genetic Diversity", gp=gpar(fontsize=12)),
  left ="False Positive Rate")
