#FPR by FST Scatterplot, 6 contributors
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

#Removing pops that are not part of the 26 cross analysis
pops_26 <- read.csv("C:/Users/Evan Ho/Documents/FST Lab Work/trimmed_informed_consent_groups.csv")
pops_26 <- as.vector(c(pops_26$x))
FSTarray <- FSTarray[ , pops_26] #remove columns
FSTarray <- FSTarray[pops_26 , ] #remove rows

ref_pops <- c()
sim_pops <- c()
FPRs <- c()

Files<-list.files("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/LR_Cross_26_050524_0_10000_6/")

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
LRCross_results <- data.frame(sim_pops, ref_pops, FPRs)
LRCross_matrix <- acast(LRCross_results, sim_pops~ref_pops, value.var = "FPRs")

#Supplying FPR heatmap with results from actual cross.
#getting the 26 pops
pops_aeh_sorted <- read.csv("C:/Users/Evan Ho/Documents/FST Lab Work/StraightAnalysisResults/PopAEHsSorted.csv")
pops_aeh_sorted <- pops_aeh_sorted[pops_aeh_sorted$X %in% pops_26, ]

#Setting it alphabetically
pops_aeh_sorted <- pops_aeh_sorted[order(pops_aeh_sorted$X),]
pops_aeh_sorted$AverageExpHet <- signif(pops_aeh_sorted$AverageExpHet, digits = 5)

#Renaming the columns to their AEH values
for (col in 1:length(LRCross_matrix[1,])) {
  colnames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
  rownames(LRCross_matrix)[col] <- pops_aeh_sorted$AverageExpHet[col]
}

################################################################################
#Scatterplot of using a more incorrect ref group leads to higher FPRs.

#FST long array
FSTarray_long <- FSTarray %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FST")

#FPR long array
LRCross_long <- LRCross_matrix %>%
  as.data.frame() %>%
  rownames_to_column("sim_pop") %>%
  pivot_longer(-c(sim_pop), names_to = "ref_pop", values_to = "FPRs")

#Adding FST column to FPR long array
LRCross_long <- cbind(LRCross_long, FSTarray_long$FST)
colnames(LRCross_long) <- c("sim_pop", "ref_pop", "FPRs", "FST")

#Removing direct analysis rows (when sim_pop == ref_pop)
LRCross_long <- LRCross_long[LRCross_long$sim_pop != LRCross_long$ref_pop,]

#Adding a column to check if ref_pop or sim_pop has higher AEH
AEH_diff <- c()
difference <- c()

#Creating a vector to identify whether the sim group or the ref group has higher AEH
for (AEH_compare in 1:nrow(LRCross_long)) {
  difference <- as.numeric(LRCross_long$sim_pop[AEH_compare]) - as.numeric(LRCross_long$ref_pop[AEH_compare])
  if (difference > 0) {
    difference = 1
  } else {
    difference = 0
  }
  AEH_diff <- c(AEH_diff, difference)
}

LRCross_long$AEH_diff <- c(AEH_diff)

#Calculating correlation between FPR and FST
correlation <- cor(LRCross_long$FPRs, LRCross_long$FST, method = c("pearson"))
cor.test(LRCross_long$FPRs, LRCross_long$FST)

#Scatterplot FPR over FST
FPR_FST_plot <- ggplot(LRCross_long, aes(x = FST, y = FPRs, color = factor(AEH_diff))) +
  geom_point() +
  scale_color_discrete(name = c("Genetic Diversity"), labels = c('Ref Group > Sim Group', 'Ref Group < Sim Group')) +
  ylab("False Positive Rates") +
  xlab(bquote(F[ST]))+
  theme(legend.position = c(0.15, 0.8),
        legend.title.align = 0.5,
        legend.box.background = element_rect(color="black", linewidth = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) 
print(FPR_FST_plot)

#Same as above, except removing the color component of the dots
#and adding a regression line
FPR_FST_plot <- ggplot(LRCross_long, aes(x = FST, y = FPRs)) +
  geom_point(size = 3) +
  ylab("False Positive Rate") +
  xlab(bquote(F[ST]))+
  theme(legend.position = c(0.15, 0.8),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 14),
        legend.box.background = element_rect(color="black", linewidth = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  stat_smooth(method = "lm",
              linewidth = 4,
              formula = y ~ x,
              geom = "smooth",
              se = FALSE)
print(FPR_FST_plot)

#Transforming sim_pop to continuous
LRCross_long_copy <- LRCross_long
LRCross_long_copy$sim_pop <- as.numeric(as.character(LRCross_long_copy$sim_pop))

#Same as above, trying to expand on data
FPR_FST_plot <- ggplot(LRCross_long, aes(x = FST, y = FPRs, color = sim_pop,  shape = factor(AEH_diff))) +
  geom_point(size = 3) +
  ylab("False Positive Rate") +
  xlab(bquote(F[ST]))+
  theme(legend.position = c(0.15, 0.8),
        legend.title.align = 0.5,
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.box.background = element_rect(color="black", linewidth = 1),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_color_discrete()

print(FPR_FST_plot)

#Inverted Version of the graph above
FPR_FST_plot <- ggplot(LRCross_long, aes(x = FST, y = FPRs, color = sim_pop,  shape = factor(AEH_diff))) +
  geom_point(size = 3) +
  ylab("False Positive Rate") +
  xlab(bquote(F[ST]))+
  theme(legend.position = c(0.15, 0.8),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 16, color = "white"),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "black"),
        axis.title = element_text(face = "bold", size = 20, color = "white"),
        axis.text = element_text(face = "bold", size = 20, color = "white"),
        plot.background = element_rect(fill = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "white")) +
        scale_color_discrete(direction = -1)

print(FPR_FST_plot)
