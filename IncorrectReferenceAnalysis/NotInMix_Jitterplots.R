#NotInMix Jitterplot comparing high and low GD groups
#Updated 8/8/2024

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

setwd("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/")

#cross analysis
#low AEH sim, high AEH ref
FBINavajo.2.cross <- readRDS("LR_Cross_26_050524_0_10000_2/LRCross_FBINavajo_expanded_new_seed_10_10000_2_contribNotINmix.rds")
FBINavajo.3.cross <- readRDS("LR_Cross_26_050524_0_10000_3/LRCross_FBINavajo_expanded_new_seed_10_10000_3_contribNotINmix.rds")
FBINavajo.4.cross <- readRDS("LR_Cross_26_050524_0_10000_4/LRCross_FBINavajo_expanded_new_seed_10_10000_4_contribNotINmix.rds")
FBINavajo.5.cross <- readRDS("LR_Cross_26_050524_0_10000_5/LRCross_FBINavajo_expanded_new_seed_10_10000_5_contribNotINmix.rds")
FBINavajo.6.cross <- readRDS("LR_Cross_26_050524_0_10000_6/LRCross_FBINavajo_expanded_new_seed_10_10000_6_contribNotINmix.rds")

#modifications to data done on each line, select only the lines that are needed
FBINavajo.2.cross <- FBINavajo.2.cross$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
FBINavajo.2.cross <- ifelse(FBINavajo.2.cross < 0, 0, FBINavajo.2.cross) #all values < -20 are set to -20.

FBINavajo.3.cross <- FBINavajo.3.cross$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
FBINavajo.3.cross <- ifelse(FBINavajo.3.cross < 0, 0, FBINavajo.3.cross)

FBINavajo.4.cross <- FBINavajo.4.cross$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
FBINavajo.4.cross <- ifelse(FBINavajo.4.cross < 0, 0, FBINavajo.4.cross)

FBINavajo.5.cross <- FBINavajo.5.cross$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
FBINavajo.5.cross <- ifelse(FBINavajo.5.cross < 0, 0, FBINavajo.5.cross)

FBINavajo.6.cross <- FBINavajo.6.cross$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
FBINavajo.6.cross <- ifelse(FBINavajo.6.cross < 0, 0, FBINavajo.6.cross)

#high AEH sim, low AEH ref
FBITrinidad.2.cross <- readRDS("LR_Cross_26_050524_0_10000_2/LRCross_FBITrinidad_expanded_new_seed_10_10000_2_contribNotINmix.rds")
FBITrinidad.3.cross <- readRDS("LR_Cross_26_050524_0_10000_3/LRCross_FBITrinidad_expanded_new_seed_10_10000_3_contribNotINmix.rds")
FBITrinidad.4.cross <- readRDS("LR_Cross_26_050524_0_10000_4/LRCross_FBITrinidad_expanded_new_seed_10_10000_4_contribNotINmix.rds")
FBITrinidad.5.cross <- readRDS("LR_Cross_26_050524_0_10000_5/LRCross_FBITrinidad_expanded_new_seed_10_10000_5_contribNotINmix.rds")
FBITrinidad.6.cross <- readRDS("LR_Cross_26_050524_0_10000_6/LRCross_FBITrinidad_expanded_new_seed_10_10000_6_contribNotINmix.rds")

FBITrinidad.2.cross <- FBITrinidad.2.cross$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
FBITrinidad.2.cross <- ifelse(FBITrinidad.2.cross < 0, 0, FBITrinidad.2.cross) #all values < -20 are set to -20.

FBITrinidad.3.cross <- FBITrinidad.3.cross$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
FBITrinidad.3.cross <- ifelse(FBITrinidad.3.cross < 0, 0, FBITrinidad.3.cross)

FBITrinidad.4.cross <- FBITrinidad.4.cross$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
FBITrinidad.4.cross <- ifelse(FBITrinidad.4.cross < 0, 0, FBITrinidad.4.cross)

FBITrinidad.5.cross <- FBITrinidad.5.cross$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
FBITrinidad.5.cross <- ifelse(FBITrinidad.5.cross < 0, 0, FBITrinidad.5.cross)

FBITrinidad.6.cross <- FBITrinidad.6.cross$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
FBITrinidad.6.cross <- ifelse(FBITrinidad.6.cross < 0, 0, FBITrinidad.6.cross)

#direct analysis, they are in alphabetical order
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/NotinMix_050524/")
Direct_analysis <- readRDS("Direct_not_in_mix_3dresults_051724.RDS")

#low AEH 35 is FBINavajo
FBINavajo.2.direct <- Direct_analysis[, 1, 35]
FBINavajo.2.direct <- ifelse(FBINavajo.2.direct < 0, 0, FBINavajo.2.direct) #values below -20, set to -20

FBINavajo.3.direct <- Direct_analysis[, 2, 35]
FBINavajo.3.direct <- ifelse(FBINavajo.3.direct < 0, 0, FBINavajo.3.direct)

FBINavajo.4.direct <- Direct_analysis[, 3, 35]
FBINavajo.4.direct <- ifelse(FBINavajo.4.direct < 0, 0, FBINavajo.4.direct)

FBINavajo.5.direct <- Direct_analysis[, 4, 35]
FBINavajo.5.direct <- ifelse(FBINavajo.5.direct < 0, 0, FBINavajo.5.direct)

FBINavajo.6.direct <- Direct_analysis[, 5, 35]
FBINavajo.6.direct <- ifelse(FBINavajo.6.direct < 0, 0, FBINavajo.6.direct)

#high AEH 38 is FBITrinidad
FBITrinidad.2.direct <- Direct_analysis[, 1, 38]
FBITrinidad.2.direct <- ifelse(FBITrinidad.2.direct < 0, 0, FBITrinidad.2.direct)

FBITrinidad.3.direct <- Direct_analysis[, 2, 38]
FBITrinidad.3.direct <- ifelse(FBITrinidad.3.direct < 0, 0, FBITrinidad.3.direct)

FBITrinidad.4.direct <- Direct_analysis[, 3, 38]
FBITrinidad.4.direct <- ifelse(FBITrinidad.4.direct < 0, 0, FBITrinidad.4.direct)

FBITrinidad.5.direct <- Direct_analysis[, 4, 38]
FBITrinidad.5.direct <- ifelse(FBITrinidad.5.direct < 0, 0, FBITrinidad.5.direct)

FBITrinidad.6.direct <- Direct_analysis[, 5, 38]
FBITrinidad.6.direct <- ifelse(FBITrinidad.6.direct < 0, 0, FBITrinidad.6.direct)

#saving space
rm(Direct_analysis)

#pop names
high_aeh_pop <- c("FBITrinidad")
low_aeh_pop <- c("FBINavajo")

#names of crosses
all_sim_pops <- rep(c(high_aeh_pop, low_aeh_pop), each = 550000)
all_ref_pops <- rep(c(high_aeh_pop, low_aeh_pop, low_aeh_pop, high_aeh_pop), c(500000, 50000, 500000, 50000))
contribs_straight <- rep(c(2, 3, 4, 5, 6), each = 100000)
contribs_cross <- rep(c(2, 3, 4, 5, 6), each = 10000)
contribs <- c(contribs_straight, contribs_cross,
              contribs_straight, contribs_cross)

#creating the faceted groups
#Direct High AEH
FBITrinidad.direct <- c(FBITrinidad.2.direct, FBITrinidad.3.direct,
                        FBITrinidad.4.direct, FBITrinidad.5.direct,
                        FBITrinidad.6.direct)
FBITrinidad_matrix <- cbind(all_sim_pops[1:500000], all_ref_pops[1:500000],
                            contribs[1:500000], FBITrinidad.direct)
FBITrinidad_matrix <- data.frame(FBITrinidad_matrix)

#high sim low ref
high_sim_low_ref <- c(FBITrinidad.2.cross, FBITrinidad.3.cross,
                      FBITrinidad.4.cross, FBITrinidad.5.cross,
                      FBITrinidad.6.cross)
high_sim_low_ref_matrix <- cbind(all_sim_pops[500001:550000], all_ref_pops[500001:550000],
                                 contribs[500001:550000], high_sim_low_ref)
high_sim_low_ref_matrix <- data.frame(high_sim_low_ref_matrix)

#Direct Low AEH
FBINavajo.direct <- c(FBINavajo.2.direct, FBINavajo.3.direct,
                      FBINavajo.4.direct, FBINavajo.5.direct,
                      FBINavajo.6.direct)
FBINavajo_matrix <- cbind(all_sim_pops[550001:1050000], all_ref_pops[550001:1050000],
                          contribs[550001:1050000], FBINavajo.direct)
FBINavajo_matrix <- data.frame(FBINavajo_matrix)

#low sim high ref
low_sim_high_ref <- c(FBINavajo.2.cross, FBINavajo.3.cross,
                      FBINavajo.4.cross, FBINavajo.5.cross,
                      FBINavajo.6.cross)
low_sim_high_ref_matrix <- cbind(all_sim_pops[1050001:1100000], all_ref_pops[1050001:1100000],
                                 contribs[1050001:1100000], low_sim_high_ref)
low_sim_high_ref_matrix <- data.frame(low_sim_high_ref_matrix)

#combined results
FPR_results <- c(FBITrinidad.2.direct, FBITrinidad.3.direct, FBITrinidad.4.direct, FBITrinidad.5.direct, FBITrinidad.6.direct,  
                 FBITrinidad.2.cross, FBITrinidad.3.cross, FBITrinidad.4.cross, FBITrinidad.5.cross, FBITrinidad.6.cross,
                 FBINavajo.2.direct, FBINavajo.3.direct, FBINavajo.4.direct, FBINavajo.5.direct, FBINavajo.6.direct,
                 FBINavajo.2.cross, FBINavajo.3.cross, FBINavajo.4.cross, FBINavajo.5.cross, FBINavajo.6.cross)
FPR_matrix <- cbind(all_sim_pops, all_ref_pops, contribs, FPR_results)
FPR_matrix <- data.frame(FPR_matrix)

#creating a column to group the analysis and reorder the groups
straight_high <- rep(c("High GD & High GD"), 500000)
straight_low <- rep(c("Low GD & Low GD"), 500000)
cross_high_low <- rep(c("High GD & Low GD"), 50000)
cross_low_high <- rep(c("Low GD & High GD"), 50000)
analysis <- c(straight_high, cross_high_low,
              straight_low, cross_low_high)
FPR_matrix$cross <- analysis

#reorder the groups
FPR_matrix_copy <- FPR_matrix
FPR_matrix$cross <- factor(FPR_matrix_copy$cross,
                           c("High GD & High GD",
                             "High GD & Low GD",
                             "Low GD & Low GD",
                             "Low GD & High GD"))


#subset without the zero values
FPR_boxplots <- ggplot(subset(FPR_matrix, !(FPR_results == 0)),
                       aes(x = contribs,
                           y = as.numeric(FPR_results),
                           color = cross,
                           group = interaction(cross, contribs))) +
  geom_point(position = position_jitterdodge())+
  scale_color_manual(cross, values = c("red3", "deeppink1", "navyblue", "royalblue1")) +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.background = element_rect(fill = "white", colour = 1),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  xlab("Number of Contributors") +
  ylab("log(LRs)") 
print(FPR_boxplots)


#subset without the zero values -- inverted
FPR_boxplots <- ggplot(subset(FPR_matrix, !(FPR_results == 0)),
                       aes(x = contribs,
                           y = as.numeric(FPR_results),
                           color = cross,
                           group = interaction(cross, contribs))) +
  geom_point(position = position_jitterdodge())+#position = position_dodge(0.8), width = 0.7) +
  #  scale_y_continuous(trans = "log10") +
  #  coord_cartesian(ylim = c(0, 101)) +
  scale_color_manual(cross, values = c("red3", "deeppink1", "steelblue1", "turquoise1")) +
  #theme_linedraw() +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill = "black", colour = "white"),
        legend.text = element_text(face = "bold", color = "white"),
        legend.background = element_rect(fill = "black", colour = 1),
        axis.title = element_text(face = "bold", color = "white"),
        axis.text.x = element_text(face = "bold", size = 12, color = "white"),
        axis.text.y = element_text(face = "bold", size = 12, color = "white"),
        panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(color = "white"), 
        plot.background = element_rect(fill = "black")) +
  xlab("Number of Contributors") +
  ylab("log(LRs)") 
print(FPR_boxplots)
