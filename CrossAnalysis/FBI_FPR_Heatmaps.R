#FPR Heatmap for FBI pops only, all contributors
#Updated 8/7/2024

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
library(scales)
library(lemon)

#Setting working directory
setwd("C:/Users/Evan Ho/Documents/FST Lab Work")

#Read in the FST array
FSTarray <- read.csv("FSTarrayinformedconsent.csv")
informed_consent_groups <- read.csv("informed_consent_groups.csv")

#Set first column as row names
rownames(FSTarray) <- informed_consent_groups$x
colnames(FSTarray) <- informed_consent_groups$x

#Removing groups that are not part of the cross analysis
pops_26 <- read.csv("C:/Users/Evan Ho/Documents/FST Lab Work/trimmed_informed_consent_groups.csv")
pops_26 <- as.vector(c(pops_26$x))
FSTarray <- FSTarray[ , pops_26] #remove columns
FSTarray <- FSTarray[pops_26 , ] #remove rows

ref_pops <- c()
sim_pops <- c()
FPRs <- c()

Files<-list.files("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/LR_Cross_26_050524_0_10000_6")

for (j in 1:length(Files)) {
  current_file <-readRDS(paste0("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/LR_Cross_26_050524_0_10000_6/", Files[j]))
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
LRCross_results_6 <- data.frame(sim_pops, ref_pops, FPRs)
LRCross_matrix_6 <- acast(LRCross_results_6, sim_pops~ref_pops, value.var = "FPRs")

#getting the cross analysis groups
pops_aeh_sorted <- read.csv("C:/Users/Evan Ho/Documents/FST Lab Work/StraightAnalysisResults/PopAEHsSorted.csv")
pops_aeh_sorted <- pops_aeh_sorted[pops_aeh_sorted$X %in% pops_26, ]

pops_aeh_sorted <- pops_aeh_sorted[order(pops_aeh_sorted$X),]
pops_aeh_sorted$AverageExpHet <- signif(pops_aeh_sorted$AverageExpHet, digits = 5)


for (col in 1:length(LRCross_matrix_6[1,])) {
  rownames(LRCross_matrix_6)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

#Removing non-FBI ref groups
LRCross_matrix_6 <- LRCross_matrix_6[,-c(1:5)]
LRCross_matrix_6 <- LRCross_matrix_6[,-c(9:21)]

#Data prep for heatmap
LRCross_long_6 <- LRCross_matrix_6 %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FPRs")

#Another method where all long arrays are combined into one
num_contribs <- rep(c("6 Contributors"), c(208))
LRCross_long_6$num_contrib <- num_contribs

#Repeat above code for 2-6 contributors before continuing on with the code below.

all_LRCross_long <- rbind(LRCross_long_2, LRCross_long_3,
                          LRCross_long_4, LRCross_long_5,
                          LRCross_long_6)

LRCross_heatmap_plot <- ggplot(data = all_LRCross_long, aes(x = ref_pop, y = sim_pop)) +
  geom_tile(aes(fill = FPRs), color = "black") +
  theme(legend.position = "left", 
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(hjust = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(-0.5, "lines"),
        strip.background = element_rect(fill="white")) +
  scale_x_discrete(labels = c("African American", "Apache",
                              "Caucasian", "Filipino",
                              "Navajo", "SE Hispanic",
                              "SW Hispanic", "Trinidadian")) +
  scale_fill_continuous(limits = c(0, 0.063), low = "white", high = "#010032") +
  guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                ticks.colour = "black",
                                ticks.linewidth = 0.5,
                                frame.colour = "black",
                                frame.linewidth = 0.5)) + 
  xlab("Reference Group") +
  ylab("Simulation Group") +
  facet_rep_wrap(~num_contrib, ncol = 1, repeat.tick.labels = 'left')

print(LRCross_heatmap_plot)
