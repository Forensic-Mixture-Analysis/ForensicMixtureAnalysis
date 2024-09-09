#InMix low vs high GD boxplots
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

#directory
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/")

#Gathering the data:
direct_results <- readRDS("Direct_in_mix_3dresults_051924.RDS")

#high sim high ref: FBITrinidad, group 38
high_sim_high_ref_1 <- direct_results[, 1, 38] #all data, 1 contrib, 38 is FBITrinidad
high_sim_high_ref_2 <- direct_results[, 2, 38] #all data, 2 contrib, 38 is FBITrinidad
high_sim_high_ref_3 <- direct_results[, 3, 38] #all data, 3 contrib, 38 is FBITrinidad
high_sim_high_ref_4 <- direct_results[, 4, 38] #all data, 4 contrib, 38 is FBITrinidad
high_sim_high_ref_5 <- direct_results[, 5, 38] #all data, 5 contrib, 38 is FBITrinidad

#new high AEH sim, low AEH ref
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_2/")
high_sim_low_ref_2contrib <- readRDS("LRCross_FBITrinidad_expanded_new_seed_10_10000_2_contribINmix.rds")
high_sim_low_ref_2contrib <- high_sim_low_ref_2contrib$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_3/")
high_sim_low_ref_3contrib <- readRDS("LRCross_FBITrinidad_expanded_new_seed_10_10000_3_contribINmix.rds")
high_sim_low_ref_3contrib <- high_sim_low_ref_3contrib$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_4/")
high_sim_low_ref_4contrib <- readRDS("LRCross_FBITrinidad_expanded_new_seed_10_10000_4_contribINmix.rds")
high_sim_low_ref_4contrib <- high_sim_low_ref_4contrib$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_5/")
high_sim_low_ref_5contrib <- readRDS("LRCross_FBITrinidad_expanded_new_seed_10_10000_5_contribINmix.rds")
high_sim_low_ref_5contrib <- high_sim_low_ref_5contrib$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_6/")
high_sim_low_ref_6contrib <- readRDS("LRCross_FBITrinidad_expanded_new_seed_10_10000_6_contribINmix.rds")
high_sim_low_ref_6contrib <- high_sim_low_ref_6contrib$`FBITrinidad_expanded_new FBINavajo_expanded_new.csv`


#low sim low ref: FBINavajo, group 35
low_sim_low_ref_1 <- direct_results[, 1, 35] #all data, 1 contribs, 35 is FBINavajo
low_sim_low_ref_2 <- direct_results[, 2, 35] #all data, 2 contribs, 35 is FBINavajo
low_sim_low_ref_3 <- direct_results[, 3, 35] #all data, 3 contribs, 35 is FBINavajo
low_sim_low_ref_4 <- direct_results[, 4, 35] #all data, 4 contribs, 35 is FBINavajo
low_sim_low_ref_5 <- direct_results[, 5, 35] #all data, 5 contribs, 35 is FBINavajo

#new low AEH sim, high AEH ref
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_2/")
low_sim_high_ref_2contrib <- readRDS("LRCross_FBINavajo_expanded_new_seed_10_10000_2_contribINmix.rds")
low_sim_high_ref_2contrib <- low_sim_high_ref_2contrib$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_3/")
low_sim_high_ref_3contrib <- readRDS("LRCross_FBINavajo_expanded_new_seed_10_10000_3_contribINmix.rds")
low_sim_high_ref_3contrib <- low_sim_high_ref_3contrib$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_4/")
low_sim_high_ref_4contrib <- readRDS("LRCross_FBINavajo_expanded_new_seed_10_10000_4_contribINmix.rds")
low_sim_high_ref_4contrib <- low_sim_high_ref_4contrib$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_5/")
low_sim_high_ref_5contrib <- readRDS("LRCross_FBINavajo_expanded_new_seed_10_10000_5_contribINmix.rds")
low_sim_high_ref_5contrib <- low_sim_high_ref_5contrib$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`
setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/LR_Cross_26_050524_1_10000_6/")
low_sim_high_ref_6contrib <- readRDS("LRCross_FBINavajo_expanded_new_seed_10_10000_6_contribINmix.rds")
low_sim_high_ref_6contrib <- low_sim_high_ref_6contrib$`FBINavajo_expanded_new FBITrinidad_expanded_new.csv`

#saving some R space
rm(direct_results)

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
#high sim high ref
high_sim_high_ref <- c(high_sim_high_ref_1, high_sim_high_ref_2,
                       high_sim_high_ref_3, high_sim_high_ref_4,
                       high_sim_high_ref_5)
high_sim_high_ref_matrix <- cbind(all_sim_pops[1:500000], all_ref_pops[1:500000],
                                  contribs[1:500000], high_sim_high_ref)
high_sim_high_ref_matrix <- data.frame(high_sim_high_ref_matrix)


#high sim low ref
high_sim_low_ref <- c(high_sim_low_ref_2contrib, high_sim_low_ref_3contrib,
                      high_sim_low_ref_4contrib, high_sim_low_ref_5contrib,
                      high_sim_low_ref_6contrib)
high_sim_low_ref_matrix <- cbind(all_sim_pops[500001:550000], all_ref_pops[500001:550000],
                                 contribs[500001:550000], high_sim_low_ref)
high_sim_low_ref_matrix <- data.frame(high_sim_low_ref_matrix)


#low sim low ref
low_sim_low_ref <- c(low_sim_low_ref_1, low_sim_low_ref_2,
                     low_sim_low_ref_3, low_sim_low_ref_4,
                     low_sim_low_ref_5)
low_sim_low_ref_matrix <- cbind(all_sim_pops[550001:1050000], all_ref_pops[550001:1050000],
                                contribs[550001:1050000], low_sim_low_ref)
low_sim_low_ref_matrix <- data.frame(low_sim_low_ref_matrix)


#low sim high ref
low_sim_high_ref <- c(low_sim_high_ref_2contrib, low_sim_high_ref_3contrib,
                      low_sim_high_ref_4contrib, low_sim_high_ref_5contrib,
                      low_sim_high_ref_6contrib)
low_sim_high_ref_matrix <- cbind(all_sim_pops[1050001:1100000], all_ref_pops[1050001:1100000],
                                 contribs[1050001:1100000], low_sim_high_ref)
low_sim_high_ref_matrix <- data.frame(low_sim_high_ref_matrix)


#combined results
power_results <- c(high_sim_high_ref_1, high_sim_high_ref_2, high_sim_high_ref_3, high_sim_high_ref_4, high_sim_high_ref_5,  
                   high_sim_low_ref_2contrib, high_sim_low_ref_3contrib, high_sim_low_ref_4contrib, high_sim_low_ref_5contrib, high_sim_low_ref_6contrib,
                   low_sim_low_ref_1, low_sim_low_ref_2, low_sim_low_ref_3, low_sim_low_ref_4, low_sim_low_ref_5,
                   low_sim_high_ref_2contrib, low_sim_high_ref_3contrib, low_sim_high_ref_4contrib, low_sim_high_ref_5contrib, low_sim_high_ref_6contrib)
power_matrix <- cbind(all_sim_pops, all_ref_pops, contribs, power_results)
power_matrix <- data.frame(power_matrix)

#creating a column to group the analysis and reorder the boxplots
straight_high <- rep(c("High GD & High GD"), 500000)
straight_low <- rep(c("Low GD & Low GD"), 500000)
cross_high_low <- rep(c("High GD & Low GD"), 50000)
cross_low_high <- rep(c("Low GD & High GD"), 50000)
analysis <- c(straight_high, cross_high_low,
              straight_low, cross_low_high)
power_matrix$cross <- analysis

power_matrix_copy <- power_matrix

#reorder the groups for the boxplots
power_matrix$cross <- factor(power_matrix_copy$cross,
                             c("High GD & High GD",
                               "High GD & Low GD",
                               "Low GD & Low GD",
                               "Low GD & High GD"))

power_boxplots <- ggplot(power_matrix,
                         aes(x = contribs,
                             y = as.numeric(power_results),
                             fill = cross,
                             group = interaction(cross, contribs))) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = c("red3", "pink", "steelblue4", "paleturquoise1")) +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(face = "bold"),
        legend.background = element_rect(fill = "white", colour = 1),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  xlab("Number of Contributors") +
  ylab("log(LRs)") 
print(power_boxplots)

################################################################################
#Figuring out where the false negatives are in the direct analysis
#directory
#setwd("C:/Users/Evan Ho/Documents/FST Lab Work/InMix_050524/")

#Gathering the data:
#direct_results <- readRDS("Direct_in_mix_3dresults_051924.RDS")

#sum(direct_results[,5,] < 0) reveals it is in the 6 contributor mixture, now identify which groups
#x = 1
#for (n in 1:83) {
#  if (sum(direct_results[,5,n] < 0)) {
#    print(n)
#    print(sum(direct_results[, 5, n] < 0))
#  }
#}
