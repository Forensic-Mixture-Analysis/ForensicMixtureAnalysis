#Low Template Mixture Analysis - Cross Analysis Heatmap

#Necessary Libraries
library(ggplot2)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggdendro)
library(grid)
library(gridExtra)
library(stringr)
library(cowplot)

#Setting working directory
setwd("C:/Users/evanh/Documents/FST Lab Work")

#Read in the FST array
FSTarray <- read.csv("newFSTarray.csv")

#Set first column as row names
rownames(FSTarray) <- FSTarray$X
#Remove the first column
FSTarray <- FSTarray[ , -1]

#Removing pops that are not part of the 54 cross analysis
pops_54 <- read.csv("C:/Users/evanh/Documents/FST Lab Work/pop_results/54_pops_to_run.csv")
pops_54 <- as.vector(c(pops_54$x))
FSTarray <- FSTarray[ , pops_54] #remove columns
FSTarray <- FSTarray[pops_54 , ] #remove rows

#Empty vectors to be filled
ref_pops <- c()
sim_pops <- c()
FPRs <- c()

Files<-list.files("C:/Users/evanh/Documents/FST Lab Work/CrossAnalysisResults/LRCross_not_inmix",pattern="10k_6contribmix.rds")

for (j in 1:length(Files)) {
  current_file <-readRDS(paste0("C:/Users/evanh/Documents/FST Lab Work/CrossAnalysisResults/LRCross_not_inmix/", Files[j]))
  for (i in 1:length(current_file[])) {
    current_cross <- names(current_file[i])
    current_sim_pop <- word(current_cross, 1)
    current_ref_pop <- word(current_cross, -1)
    cross_result <- sum(current_file[[i]] > 0) / length(current_file[[i]])
    ref_pops <- c(ref_pops, current_ref_pop)
    sim_pops <- c(sim_pops, current_sim_pop)
    FPRs <- c(FPRs, cross_result)
  }
}
LRCross_results <- data.frame(sim_pops, ref_pops, FPRs)
LRCross_matrix <- acast(LRCross_results, sim_pops~ref_pops, value.var = "FPRs")

#Supplying FPR heatmap with results from actual cross.
setwd("C:/Users/evanh/Documents/FST Lab Work/StraightAnalysisResults/LR_Direct_1")

straight_results <- readRDS("LR_Direct_1_6contrib_Results.RDS")

#getting the 54 pops
pops_aeh_sorted <- read.csv("C:/Users/Evan Ho/Documents/FST Lab Work/StraightAnalysisResults/PopAEHsSorted.csv")
pops_aeh_sorted <- pops_aeh_sorted[pops_aeh_sorted$X %in% pops_54, ]

#getting the straight analysis results
straight_results <- straight_results[ , c(pops_aeh_sorted$X.1)]
colnames(straight_results) <- pops_aeh_sorted$X

#sorting the columns alphabetically
straight_results <- straight_results[,order(colnames(straight_results))]

#calculating the FPRs
straight_FPRs <- c()

for (col in 1:length(straight_results[1,])) {
  col_result <- sum(straight_results[,col] > 0) / 100000
  straight_FPRs <- c(straight_FPRs, col_result)
}

#filling in the diagonal values with actual FPR values
diag(LRCross_matrix) <- straight_FPRs

#sorting and renaming the groups to their genetic diversities
pops_aeh_sorted <- pops_aeh_sorted[order(pops_aeh_sorted$X),]
pops_aeh_sorted$AverageExpHet <- signif(pops_aeh_sorted$AverageExpHet, digits = 5)

for (col in 1:length(straight_results[1,])) {
  colnames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Data prep for heatmap
LRCross_long <- LRCross_matrix %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FPRs")

#Initial heatmap
LRCross_heatmap_plot <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FPRs)) +
  theme(legend.position = "left", 
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))+
  xlab("Reference Group GD") +
  ylab("Simulation Group GD")

print(LRCross_heatmap_plot)

#Clustering and reordering of the heatmap so that it is grouped by FST:

#Updating Dendrogram with AEH labels
for (col in 1:length(FSTarray[1,])) {
  colnames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Clustering
FSTdendro <- as.dendrogram(hclust(d = dist(x = FSTarray)))
dendro_plot <- ggdendrogram(data = FSTdendro, rotate = TRUE) +
  theme(text = element_text(size = 4))
print(dendro_plot)

#Clustering
LRCross_dendro_plot <- ggdendrogram(data = FSTdendro, rotate = TRUE) +
  theme(legend.position = "left", 
        axis.text = element_text(face = "bold"))

#Creating the order
LRCross_dendro_order <- order.dendrogram(FSTdendro)

#Fixing order of Y-axis
LRCross_long$sim_pop <- factor(x = LRCross_long$sim_pop,
                               levels = LRCross_long$ref_pop[LRCross_dendro_order],
                               ordered = TRUE)
#Fixing order of X-axis
LRCross_long$ref_pop <- factor(x = LRCross_long$ref_pop,
                               levels = LRCross_long$ref_pop[LRCross_dendro_order],
                               ordered = TRUE)

#Recreating the heatmap with the correct order
LRCross_heatmap <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FPRs)) +
  xlab("Reference Group GD") +
  ylab("Simulation Group GD") +
  theme(legend.position = "left", 
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

grid.newpage()
print(LRCross_heatmap,
      vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(LRCross_dendro_plot,
      vp = viewport(x = 0.895, y = 0.52, width = 0.2, height = 1.03))
