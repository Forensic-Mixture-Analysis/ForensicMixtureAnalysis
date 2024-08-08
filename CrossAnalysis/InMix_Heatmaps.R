#InMix Heatmaps (one for displaying number of false negatives, another for displaying
#the average log(LRs))
#Updated 8/8/2024

#Necessary libraries
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

#Read in the FST array
FSTarray <- read.csv("FSTarrayinformedconsent_named.csv")

#Set first column as row names
rownames(FSTarray) <- FSTarray$X
#Remove the first column
FSTarray <- FSTarray[ , -1]

#Removing groups that are not part of the cross analysis
pops_26 <- read.csv("trimmed_informed_consent_groups.csv")
pops_26 <- as.vector(c(pops_26$x))
FSTarray <- FSTarray[ , pops_26] #remove columns
FSTarray <- FSTarray[pops_26 , ] #remove rows

#getting the 26 groups
pops_aeh_sorted <- read.csv("GroupAEHs.csv")
pops_aeh_sorted <- pops_aeh_sorted[pops_aeh_sorted$X %in% pops_26, ]

pops_aeh_sorted$AverageExpHet <- signif(pops_aeh_sorted$AverageExpHet, digits = 5)

#Updating FSTarray with AEH labels
for (col in 1:length(FSTarray[1,])) {
  colnames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(FSTarray)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

################################################################################
ref_pops <- c()
sim_pops <- c()
FNs <- c()

Files<-list.files("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_4/")

for (j in 1:length(Files)) {
  current_file <-readRDS(paste0("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_4/", Files[j]))
  for (i in 1:length(Files)) {
    current_cross <- names(current_file[i])
    current_sim_pop <- word(current_cross, 1)
    current_sim_pop <- sub("_new","", current_sim_pop)      #remove extra stuff from file name
    current_ref_pop <- word(current_cross, -1)
    current_ref_pop <- sub("_new.csv","", current_ref_pop)      #remove extra stuff from file name
    cross_result <- sum(current_file[[i]] < 6) / length(current_file[[i]])
    ref_pops <- c(ref_pops, current_ref_pop)
    sim_pops <- c(sim_pops, current_sim_pop)
    FNs <- c(FNs, cross_result)
  }
}
LRCross_results <- data.frame(sim_pops, ref_pops, FNs)
LRCross_matrix <- acast(LRCross_results, sim_pops~ref_pops, value.var = "FNs")

#replacing group names with their respective AEHs
for (col in 1:length(LRCross_matrix[1,])) {
  colnames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Data prep for heatmap
LRCross_long <- LRCross_matrix %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FNs")

#All heatmaps scaled to the 6-contrib FPR heatmap. Make sure to change scaling to the 6 contributor!!!
LRCross_heatmap_plot <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FNs), color = "black") +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "#010032") +   #This line scales it to the 6 contributor heatmap
  theme(legend.position = "left", 
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))+
  guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                ticks.colour = "black",
                                ticks.linewidth = 0.5,
                                frame.colour = "black",
                                frame.linewidth = 0.5)) +  
  
  xlab("Reference Group GD") +
  ylab("Simulation Group GD")

print(LRCross_heatmap_plot)

################################################################################
#Finding the average log(LRs) of each cross instead of the Power

ref_pops <- c()
sim_pops <- c()
avg_logLRs <- c()

Files<-list.files("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_2/")

for (j in 1:length(Files)) {
  current_file <-readRDS(paste0("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_2/", Files[j]))
  for (i in 1:length(Files)) {
    current_cross <- names(current_file[i])
    current_sim_pop <- word(current_cross, 1)
    current_sim_pop <- sub("_new","", current_sim_pop)      #remove extra stuff from file name
    current_ref_pop <- word(current_cross, -1)
    current_ref_pop <- sub("_new.csv","", current_ref_pop)      #remove extra stuff from file name
    cross_result <- sum(current_file[[i]]) / length(current_file[[i]])
    ref_pops <- c(ref_pops, current_ref_pop)
    sim_pops <- c(sim_pops, current_sim_pop)
    avg_logLRs <- c(avg_logLRs, cross_result)
  }
}
LRCross_results <- data.frame(sim_pops, ref_pops, avg_logLRs)
LRCross_matrix <- acast(LRCross_results, sim_pops~ref_pops, value.var = "avg_logLRs")

#replacing group names with their respective AEHs
for (col in 1:length(LRCross_matrix[1,])) {
  colnames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Data prep for heatmap
LRCross_long <- LRCross_matrix %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "avg_logLRs")

#All heatmaps scaled to the 6-contrib FPR heatmap. Make sure to change scaling to the 6 contributor!!!
LRCross_heatmap_plot <- ggplot(data = LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = avg_logLRs), color = "black") +
  scale_fill_continuous( low = "white", high = "#010032") +   #This line scales it to the 6 contributor heatmap
  theme(legend.position = "left", 
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))+
  guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                ticks.colour = "black",
                                ticks.linewidth = 0.5,
                                frame.colour = "black",
                                frame.linewidth = 0.5,
                                guide_legend(title = "Average log(LRs)"))) +  
  
  xlab("Reference Group GD") +
  ylab("Simulation Group GD")

print(LRCross_heatmap_plot)
