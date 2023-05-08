########################### SET WORKING DIRECTORY ##############################
setwd("~/OneDrive - San Francisco State University/LTA_LABWORK/244_Pops_Scripts")
########################### LOAD LIBRARIES AND PACKAGES #########################
library(ggplot2)
library(tidyr)
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
########################### LOAD 244 POP ~ IN MIX ~ DATA ########################
Power_Rates = readRDS("~/OneDrive - San Francisco State University/LTA_LABWORK/244_Pops_Scripts/pops.in.mix.no.theta.results.RDS")
########################### LOAD DATA NOT IN MIX ###########################################
popLRS <- readRDS("LR.244.pops.NOT.in.mix.results.RDS") #saves file to a variable 
popLRS[popLRS== -Inf] = -10000 #change all -INFs into -10000
# load the recalculated chubut pop LR data 
chubut_simulations = readRDS("chubut.250_new_70_not_in_mix.RDS")
chubut_simulations[chubut_simulations == -Inf] = -10000
################## TURN INTO DATAFRAME #########################################
#  LRs of All Populations
total_pops <-{
  data1 <- popLRS[,,1:20]
  data2 <- popLRS[,,21:40]
  data3 <- popLRS[,,41:60]
  data4 <- popLRS[,,61:80]
  data5 <- popLRS[,,81:100]
  data6 <- popLRS[,,101:120]
  data7 <- popLRS[,,121:140]
  data8 <- popLRS[,,141:180]
  data9 <- popLRS[,,181:200]
  data11 <- popLRS[,,201:220]
  data12 <- popLRS[,,221:244]
  
}

# Combining the data
total_pops <- abind(data1, data2, data3, data4, data5, data6, data7, data8,
                      data9, data11, data12)

# Using pops.for.sims to put names to rows for the data frame
pops.for.sims <- readRDS("pops.for.sims.RDS")
rows <- pops.for.sims

# Creating matrix: x is # of contributors, y is population
# nrows = number of pops used
FPRMatrix <- matrix(nrow=244, ncol = 5)
for (i in 1:dim(FPRMatrix)[1]) {
  for (j in 1:dim(FPRMatrix)[2]){
    FPRMatrix[i,j] = sum((total_pops[,j,i]>0))/100000
  }
}
rownames(FPRMatrix) <- pops.for.sims# assign row names to the matrix for FPRs 

colnames(FPRMatrix)<- c(c(2,3,4,5,6)) # assign column names for FPRS 

#Convert to data frame
FPR_DF <- as.data.frame(FPRMatrix)
colnames(FPR_DF) <- c(c(2,3,4,5,6)) # column names = number of contributors
rownames(FPR_DF) <- c(rows) # rows = name of populations 
FPR_DF = FPR_DF[,-1] # drop two contribs column

# Transpose the data frame
FPR_DF <- as.data.frame(t(FPR_DF)) # this flips rows and columns
FPR_DF$contribs <- rownames(FPR_DF) # assign a key name to call on specific groups

FPR_DF <- melt(FPR_DF, id.vars='contribs') # makes key name accessible when graphing

############################# BUILD 8 POPS DATA FRAME ##########################
AEH_Pops <-{
  dataE1 <- popLRS[,,220] # Tojolabal.240_new.csv
  
  dataE2 <- popLRS[,,68] # China.8_new.csv
  
  dataE3 <- popLRS[,,26] # Belarus.1_new.csv

  dataE4 <- chubut_simulations # Chubut.250_new.csv

 dataE5 <- popLRS[,,187] # S.Africa.1_new.csv
  
  dataE6 <- popLRS[,,111] # India.5_new.csv

  dataE7 <- popLRS[,,13] # Argentina.4_new.csv

  dataE8 <- popLRS[,,190] #SAColoured.242_new.csv
  
}

AEH_Pops <- {
  
  AEH_Pops<- abind(dataE1, dataE2, dataE3, dataE4,dataE5, 
                             dataE6, dataE7, dataE8, along = 3)
  # Using only Eight populations
  AEHPopMatrix <- matrix(nrow=8, ncol = 5)
  for (i in 1:dim(AEHPopMatrix)[1]) {
    for (j in 1:dim(AEHPopMatrix)[2]){
      AEHPopMatrix[i,j] = sum((AEH_Pops[,j,i]>0))/100000
    }
  }
  AEHPopMatrix = AEHPopMatrix[,-1]
  AEHPopMatrix <- as.data.frame(AEHPopMatrix)
  colnames(AEHPopMatrix) <- c(c(3,4,5,6))
  rownames(AEHPopMatrix) <- c(rows[c(220,68,26,70,187,111,13,190)])

  AEHPopDF<- as.data.frame(t(AEHPopMatrix))
  AEHPopDF$contribs <- rownames(AEHPopDF)

  AEHPopDF <- melt(AEHPopDF, id.vars='contribs')
}



################ FIND AMOUNT OF LRS = x IN EACH COLUMN ########################################
for (i in 1:ncol(FPRMatrix)) {
  print(paste("column", i, ":", sum(FPRMatrix[,i] >= 0.00001)))
}

FPRMatrix[,2] >= 0.00001 # search for pops that clear this threshold 
#################### Explore the matrix ###############################
max(FPRMatrix[,5]) # returns highest values 
which(FPRMatrix == 0.03286, arr.ind = T) # locate the indices where highest val occur
pops.for.sims[155] # returns population name 
######################## CREATE LOG VIOLIN PLOT FOR ALL LRS VS NUMBER OF CONTRIBS ###############################
eight_colors = colorRampPalette(c("pink", "green")) # color gradient for 8 pops
eight_colors(8) 
p1 <- {
  ggplot() + geom_violin(data = FPR_DF, aes(x=as.numeric(contribs), y = value, 
  fill = contribs, ), scale = "width", alpha = 0.30) +  xlab("Number of contributors") + ylab("False positive rate") + 
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) + scale_fill_manual(values = c("#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8","#BCE4D8")) +
    geom_point(data = AEHPopDF, aes(x=as.numeric(contribs), y = value, color = variable)) +
    geom_line(data = AEHPopDF, aes(x=as.numeric(contribs), y = value, color = variable)) +
    xlab("Number of Contributors") + ylab("False Positive Rate") + labs(colour = "Genetic Diversity", size = 4) +
    theme(text = element_text(size = 12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 12), legend.position = c(0.20,0.8), legend.text = element_text(size =14), legend.title = element_text(size = 14), legend.box.background = element_rect(color = "black", size = 1), 
         legend.key = element_rect(fill = "white"), legend.key.height = unit(8, "pt"))  + 
    scale_color_manual(labels = c("0.674", "0.763", "0.772", "0.778", "0.782",
                                  " 0.785", " 0.788", "0.804"),
                      values = rainbow(8)) + guides(fill = "none")
}

p1 # violin plot of LRs with quantile populations 
p2 =p1 + scale_y_continuous(trans = "log10") + labs(y=expression("False Positive Rate"), 
                                                    x = expression("Number of Contributors"))
p2 # view the log violin plot 

########### Inverted color [black background] plot ###################################
p3 <- {
  ggplot() + geom_violin(data = FPR_DF, aes(x=as.numeric(contribs), y = value, 
                                           fill = contribs, ), scale = "width", alpha = 0.90) +  xlab("Number of contributors") + ylab("False positive rate") +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "white"), plot.background = element_rect("black")) + 
    scale_fill_manual(values = c("#A4A4A4","#A4A4A4","#A4A4A4","#A4A4A4","#A4A4A4","#A4A4A4")) +
    geom_point(data = AEHPopDF, aes(x=as.numeric(contribs), y = value, color = variable), size = 3) +
    geom_line(data = AEHPopDF, aes(x=as.numeric(contribs), y = value, color = variable), size = 2) +
    xlab("Number of Contributors") + ylab("False Positive Rate") + labs(colour = "Genetic Diversity", size = 4) +
    theme(text = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "gray"), axis.text = element_text(size = 16, colour = "gray"),
          legend.position = c(0.20,0.75), legend.text = element_text(size =16, color = "gray"), legend.title = element_text(color= "gray" , size = 16), 
          legend.box.background = element_rect(color = NA, size = 1), legend.background = element_rect(fill = "black", color = NA),
          legend.key = element_rect(fill = "black"), legend.key.height = unit(8, "pt"), axis.title.x = element_text(colour = "gray"),
          axis.title.y = element_text(color = "gray"))  + 
    scale_color_manual(labels = c("0.674", "0.763", "0.772", "0.778", "0.782",
                                  " 0.785", " 0.788", "0.804"),
                       values = rainbow(8)) + guides(fill = "none")
}

p3 # linear LR plot with black background
p4 =p3 + scale_y_continuous(trans = "log10") + labs(y=expression(atop("False Positive Rate")), 
                                                    x = expression("Number of Contributors")) # log plot
p4 # log violin plot with black background


############################## GENETIC DIVERSITY VS FALSE POSITIVE RATE GRAPH ###################################################################################
# load in AEH data
PopAEHs <- read.csv("~/OneDrive - San Francisco State University/LTA_LABWORK/244_Pops_Scripts/PopAEHs.csv")

# uses data from 244 FRP matrix 
Four_Contribs = as.data.frame(FPRMatrix[,3]) # 4 contributors 
Five_Contribs = as.data.frame(FPRMatrix[,4]) # 5 contributors

# create a new dataframe with all of these things as columns so they can be graphed against each other 
AEH_FPR_DF <- data.frame(Four = Four_Contribs,
                         Five = Five_Contribs,
                         AEH = PopAEHs)

###### CREATE SCATTER PLOT OF FALSE POSITIVE RATE VS GENETIC DIVERSITY  ############################################
AEH_FPR_Plot_4 <- ggplot(data = AEH_FPR_DF, aes(x =AEH_FPR_DF[,4],  y = AEH_FPR_DF[,1])) + geom_point() + labs(x = "Genetic Diversity",
                                                                                                               y = "False positive rate") + stat_smooth(method = "lm",formula = y ~ x,geom = "smooth", se= FALSE) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  + theme(text = element_text(size = 16)) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
AEH_FPR_Plot_4 # panel plot for 4 contributors, plots Genetic Diversity (x axis) vs FPR (y axis)


AEH_FPR_Plot_5 <- ggplot(data = AEH_FPR_DF, aes(x =AEH_FPR_DF[,4],  y = AEH_FPR_DF[,2])) + geom_point() + stat_smooth(method = "lm",formula = y ~ x,geom = "smooth", se= FALSE) + theme(legend.position = "none", 
     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
    axis.line = element_line(colour = "black")) + theme(text = element_text(size = 14)) + labs(x = "Genetic Diversity",
                                                                                               y = "False positive rate") + theme(text = element_text(size = 16)) +  
  theme(axis.title.x = element_blank())

AEH_FPR_Plot_5 # panel plot for 5 contributors, plots Genetic Diversity on X axis, FPR on Y axis

# create a two panel plot 

bottom <- textGrob("Genetic diversity", gp = gpar(fontsize = 20, col = "black")) # creates unified x-axis title

uni_2 <- plot_grid(AEH_FPR_Plot_4, AEH_FPR_Plot_5, nrow =2, labels = c("a","b"), align = "v") 
uni_2 # combines 4 and 5 contributor plot 
grid.arrange(uni_2,  bottom = bottom) # adds the unified x-axis title to the plot 
#################### PANEL PLOTS WITH BLACK BACKGROUND #######################################################
Inverted_5contribs_Plot <- ggplot(data = AEH_FPR_DF, aes(x =AEH_FPR_DF[,4],  y = AEH_FPR_DF[,2])) + geom_point(color = "yellow") + 
  labs(x = "Genetic diversity", y = "False positive rate") + stat_smooth(method = "lm",formula = y ~ x,geom = "smooth",
  se= FALSE) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "white"),plot.background = element_rect(fill = "black", color = NA)) +
  theme(text = element_text(size = 14)) + theme(text = element_text(size = 16), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "white"), 
  axis.text = element_text(size = 14, colour = "white"),axis.title.x = element_text(colour = "white"),
  axis.title.y = element_text(color = "white")) +  theme(axis.title.x = element_blank()) +
  theme(axis.ticks = element_line(color = "white"))

Inverted_5contribs_Plot  # view the plot 

Inverted_4contribs_Plot <-  ggplot(data = AEH_FPR_DF, aes(x =AEH_FPR_DF[,4],  y = AEH_FPR_DF[,1])) + geom_point(color = "yellow") + 
  labs(x = "Genetic diversity", y = "False positive rate") + stat_smooth(method = "lm",formula = y ~ x,geom = "smooth",
  se= FALSE) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "white"),plot.background = element_rect(fill = "black", color = NA)) +
  theme(text = element_text(size = 14)) + theme(text = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),axis.line = element_line(colour = "white"), axis.text = element_text(size = 14, colour = "white"),
  axis.title.x = element_text(colour = "white"),axis.title.y = element_text(color = "white")) +  theme( axis.title.x = element_blank(), axis.text.x = element_blank()) +
 theme(axis.ticks = element_line(color = "white"))

Inverted_4contribs_Plot  # view plot

#yleft <- textGrob("Genetic Diversity", rot = 90, gp = gpar(fontsize = 20, col= "gray"))
bottom <-textGrob("Genetic diversity", gp = gpar(fontsize = 20, col = "white"))
uni <- plot_grid(Inverted_4contribs_Plot, Inverted_5contribs_Plot, nrow = 2, labels = c("a", "b"),
      align = "hv", label_colour = "white")
unified =grid.arrange(uni, bottom = bottom)
final_inverted <- cowplot::ggdraw(unified) + 
  theme(plot.background = element_rect(fill="black", color = NA))
final_inverted  # view the plot

############### CREATE A SUPPLEMENTAL TABLE FOR POWER RATES ###########################
# create a Power Matrix 
Power_Matrix <- matrix(nrow=244, ncol = 5) 
for (i in 1:dim(Power_Matrix)[1]) {
  for (j in 1:dim(Power_Matrix)[2]){
    Power_Matrix[i,j] = sum((Power_Rates[,j,i]>0))/100000 # done to give us the power estimate
  }
}

Power_Matrix # view the matrix 
colnames(Power_Matrix) = c(2,3,4,5,6) # number of contributors 
rownames(Power_Matrix) = PopAEHs[,2] # Group Genetic Diversity 
Power_DF = as.data.frame(Power_Matrix) # Turn into dataframe 

# create a supplemental table that will be exported 
supplemental_table = matrix(ncol = 3, nrow = 4) 
Genetic_Diversity = c(0.766983,0.78318,0.777698,0.777698) 
Power_Estimate = c(0.99999,0.99999,0.99999,0.99999)
contribs = c(5,5,5,6)
supplemental_table[,1] = Genetic_Diversity
supplemental_table[,2] = Power_Estimate
supplemental_table[,3] = contribs
colnames(supplemental_table) = c("Genetic Diversity", "Power Estimate", "Contributors")
