#Low Template Mixture Analysis - Cross Analysis Heatmap
#Updated 8/3/2024
library(ggplot2)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggdendro)
library(grid)
library(gridExtra)
library(stringr)
library(cowplot)
library(scales)
library(lemon)

#Set working directory
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/")

#FPR Heatmaps
#Read in the FST array
FSTarray <- read.csv("FSTarrayinformedconsent_named.csv")

#Set first column as row names
rownames(FSTarray) <- FSTarray$X
#Remove the first column
FSTarray <- FSTarray[ , -1]

#Removing pops that are not part of the 54 cross analysis
pops_26 <- read.csv("trimmed_informed_consent_groups.csv")
pops_26 <- as.vector(c(pops_26$x))
FSTarray <- FSTarray[ , pops_26] #remove columns
FSTarray <- FSTarray[pops_26 , ] #remove rows

#getting the 26 pops
pops_aeh_sorted <- read.csv("GroupAEHs.csv")
pops_aeh_sorted <- pops_aeh_sorted[pops_aeh_sorted$X %in% pops_26, ]

pops_aeh_sorted$AverageExpHet <- signif(pops_aeh_sorted$AverageExpHet, digits = 5)

#Updating FSTarray with AEH labels
for (col in 1:length(FSTarray[1,])) {
  colnames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

################################################################################
#Creating a dataframe to store all results of a given number of contributors
ref_pops <- c()
sim_pops <- c()
FPRs <- c()
LR_threshold <- 0
num_contrib <- c("6/")
files_folder <- c("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/LR_Cross_26_050524_0_10000_")
files_folder <- paste(files_folder, num_contrib, sep = "")
list_of_files <-list.files(files_folder)

#Loop through a folder of the results, summarize results by counting the sum of FPRs over the total number
#of simulations, then add it to a vector used to build the results matrix.
for (j in 1:length(list_of_files)) {
  current_file <-readRDS(paste0(files_folder, list_of_files[j]))
  for (i in 1:length(list_of_files)) {
    current_cross <- names(current_file[i])
    current_sim_pop <- word(current_cross, 1)
    current_sim_pop <- sub("_new","", current_sim_pop)      #remove extra stuff from file name
    current_ref_pop <- word(current_cross, -1)
    current_ref_pop <- sub("_new.csv","", current_ref_pop)      #remove extra stuff from file name
    cross_result <- sum(current_file[[i]] > LR_threshold) / length(current_file[[i]])
    ref_pops <- c(ref_pops, current_ref_pop)
    sim_pops <- c(sim_pops, current_sim_pop)
    FPRs <- c(FPRs, cross_result)
  }
}
LRCross_results <- data.frame(sim_pops, ref_pops, FPRs)
LRCross_matrix <- acast(LRCross_results, sim_pops~ref_pops, value.var = "FPRs")

#replacing group names with their respective AEHs
for (col in 1:length(LRCross_matrix[1,])) {
  colnames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Data prep for heatmap
LRCross_long <- LRCross_matrix %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FPRs")

#All heatmaps scaled to the 6-contrib FPR heatmap
LRCross_heatmap_plot <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FPRs), color = "black") +
  scale_fill_continuous( low = "white", high = "#010032") +   #This line scales it to the 6 contributor heatmap
  theme(legend.position = "left", legend.text = element_text(face = "bold", size = 14, color = "black"),
        axis.title = element_text(face = "bold", size = 20, color = "black"),
        axis.text = element_text(face = "bold", size = 20, color = "black"),
        axis.text.x = element_text(angle = 90))+
  guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                ticks.colour = "black",
                                ticks.linewidth = 0.5,
                                frame.colour = "black",
                                frame.linewidth = 0.5)) +  
  
  xlab("Reference Group GD") +
  ylab("Simulation Group GD")

print(LRCross_heatmap_plot)

#Inverted version
#All heatmaps scaled to the 6-contrib FPR heatmap
LRCross_heatmap_plot <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FPRs), color = "white") +
  scale_fill_continuous( low = "black", high = "#EAFF05") +   #This line scales it to the 6 contributor heatmap
  theme(legend.position = "left", 
        legend.text = element_text(face = "bold", size = 12, color = "white"),
        legend.title = element_text(face = "bold", size = 14, color = "white"),
        legend.background = element_rect(fill = "black"),
        axis.title = element_text(face = "bold", size = 14, color = "white"),
        axis.text = element_text(face = "bold", size = 14, color = "white"),
        axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = 'black')) +
  guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                ticks.colour = "white",
                                ticks.linewidth = 0.5,
                                frame.colour = "white",
                                frame.linewidth = 0.5)) +  
  xlab("Reference Group GD") +
  ylab("Simulation Group GD")

print(LRCross_heatmap_plot)

