#Graphing AEH distribution for 244 populations and 54 populations

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)
library(gridExtra)
library(cowplot)

setwd("C:/Users/evanh/Dropbox/PC/Documents/FST Lab Work/pop_results")
AEH_file <- read.csv("PopAEHsSorted.csv")
AEH_54_file <- read.csv("54_pops_to_run.csv")

#Labeling the columns
colnames(AEH_file) <- c("popnumber", "popname", "AverageExpHet")
colnames(AEH_54_file) <- c("X", "popname")

#244 plot + beautification
AEH_244 <- ggplot() +
  geom_histogram(data = AEH_file, aes(AverageExpHet), 
                 color = "blue", fill = "light pink") +
  xlab("Genetic Diversity") +
  ylab("Number of Groups") 
#  Dark mode
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_rect(fill = "black"),
#        plot.background = element_rect(fill = "black"),
#        axis.text = element_text(colour = "white"),
#        axis.title = element_text(colour = "white"),
#        axis.line = element_line(colour = "white"))

AEH_244

#Trimming it down to 54 pops
pops_to_remove <- setdiff(AEH_file$popname, AEH_54_file$popname)
AEH_54 <- AEH_file[!AEH_file$popname %in% pops_to_remove,]

AEH_54 <- ggplot() +
  geom_histogram(data = AEH_54, aes(AverageExpHet),
                 color = "blue", fill = "light pink") +
  xlab("Genetic Diversity") +
  ylab("Number of Groups")
#  DARK MODE
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_rect(fill = "black"),
#        plot.background = element_rect(fill = "black"),
#        axis.text = element_text(colour = "white"),
#        axis.title = element_text(colour = "white"),
#        axis.line = element_line(colour = "white"))

AEH_54
