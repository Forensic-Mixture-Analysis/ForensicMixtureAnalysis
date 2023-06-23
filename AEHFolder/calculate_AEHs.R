library(readr)

# REFERENCE VECTOR OF THE LOCI WE WANT  
loci = c('TPOX', 
         'D3S1358', 
         'FGA', 
         'CSF1PO', 
         'D5S818', 
         'D7S820', 
         'D8S1179', 
         'TH01', 
         'VWA', 
         'D13S317',
         'D16S539', 
         'D18S51',
         'D21S11')

filenames = readRDS("pops.for.sims.RDS") # READS IN FILENAMES 


OutofBoundsPops = c() # holds list of filenames whose allele frequency sum does not add to 1 or 0.99
exp_hmat.loci <- matrix(data = NA, nrow = length(filenames), ncol = length(loci)) #initiation of matrix that will hold the exp het calculations 

#assigns the col name to the matrix from the loci vector (changes)
colnames(exp_hmat.loci) <- loci

#asigns the row name to the matrix (changes)
rownames(exp_hmat.loci) <- filenames

# CODE THAT WILL CALCULATE EXPECTED HETEROZYGOSITY 

for (locus_i in 1:length(loci)){
  cur_locus <- loci[locus_i]
  
  for( population_i in 1:length(filenames)){
    AFmatrix <- as.matrix(read.csv(filenames[population_i]))
    
    for (i in 2:dim(AFmatrix)[2]){
      STRs =  colnames(AFmatrix)[i]
      
      if(STRs == cur_locus){
        AFs <- AFmatrix[,i]
        
        if (sum(as.numeric(AFs)) >= 1.01 || sum(as.numeric(AFs))<=.99){
          print(paste("out of bounds",loci[locus_i],filenames[population_i],sum(as.numeric(AFs))))
          OutofBoundsPops <- c(OutofBoundsPops, filenames[population_i])
        }
        ExpHet = (1-(sum(AFs^2)))
        exp_hmat.loci[population_i,locus_i] = ExpHet
      }
    }
  }
}
print(exp_hmat.loci)
# CODE BELOW CALCULATES AVERAGE OF EXP HET 
expHetero = exp_hmat.loci
averageExpHetero = rowMeans(expHetero)
averageExpHetOmitNA = na.omit(averageExpHetero)

# CODE BELOW WRITES AVERAGE EXP HET VALUES FOR EACH POPULATION TO CSV FILE 
averageExpHet_Mat = as.matrix(averageExpHetOmitNA, ncol=2)
colnames(averageExpHet_Mat)[1] = "AverageExpHet"
write.csv(averageExpHet_Mat, file = "PopAEHs.csv")

# SORT POPS BY AEH
popAEHs <- read.csv("PopAEHs.csv")
popAEHs <- popAEHs[order(popAEHs$AverageExpHet),]
write.csv(popAEHs, file = "PopAEHsSorted.csv")

####################################################################################### 

#######################################################################################  
