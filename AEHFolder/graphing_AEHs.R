#Updated 8/1/2024
#Graphing AEH distribution for populations and 54 populations

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)
library(gridExtra)
library(cowplot)

#set working directory
setwd("/path/to/directory")
#read in AEH file
AEH_file <- read.csv("Average_Expected_Hetero_informed_consent_with_fbi.csv")

#identify which groups should be kept
informed_consent <- c('Berber.1_new.csv', 'China.5_new.csv', 
                      'Cree.243_source_new.csv', 'Eygpt.1_new.csv',
                      'Eygpt.2_new.csv', 'Hungary.3_new.csv',
                      'Macedonia.2_new.csv', 'Maldives_new.csv',
                      'NahuaPuebla.240_new.csv', 'ParanaBrazil.241_new.csv',
                      'Romani.6_new.csv', 'S.Africa.2_new.csv', 
                      'Salishan.243_source_new.csv', 'San_new.csv', 
                      'Spain.4_new.csv', 'Tibet.6_new.csv', 'Tzeltal.240_new.csv',
                      'WNahua.240_new.csv', "FBIAfAm_expanded_new.csv",
                      "FBIApache_expanded_new.csv", "FBICauc_expanded_new.csv",
                      "FBIGuamFilipinos_expanded_new.csv", "FBINavajo_expanded_new.csv",
                      "FBISEHisp_expanded_new.csv", "FBISWHisp_expanded_new.csv",
                      "FBITrinidad_expanded_new.csv")

#create a separate dataframe for mis-specified reference group analysis
AEH_Cross <- AEH_file[AEH_file$X %in% informed_consent, ]

#Labeling the columns
colnames(AEH_file) <- c("popname", "AverageExpHet")
colnames(AEH_Cross) <- c("popname", "AverageExpHet")

#Correctly specified reference group plot
AEH_all <- ggplot() +
  geom_histogram(data = AEH_file, aes(AverageExpHet), 
                 color = "blue", fill = "light pink") +
  xlab("Genetic Diversity") +
  ylab("Number of Groups") +
#Dark mode
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(colour = "white", face = "bold"),
        axis.title = element_text(colour = "white", face = "bold"),
        axis.line = element_line(colour = "white"))

AEH_all

#Mis-specified reference group plot
AEH_cross_plot <- ggplot() +
  geom_histogram(data = AEH_Cross, aes(AverageExpHet),
                 color = "blue", fill = "light pink") +
  xlab("Genetic Diversity") +
  ylab("Number of Groups") +
#  DARK MODE
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(colour = "white", face = "bold"),
        axis.title = element_text(colour = "white", face = "bold"),
        axis.line = element_line(colour = "white"))

AEH_cross_plot
