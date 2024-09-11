################################################################################
# LOAD LIBRARY PACKAGES 
suppressPackageStartupMessages(suppressWarnings(library(tkrplot)))
library(forensim)
library(readr)
library(optparse)

#### END OF FUNCTION ###########################################################
correct_zero <- function(sim_pop_csv, ref_pop_csv, ref_pop, strs){
  
  ####################Code that impute missing AF values with 5/2N conversion ###################
  # Get imputed AF values for reference population
  ref_pop_name = as.list(strsplit(ref_pop, split = '/'))[[1]][2]
  imputed_df <- Nvalues[ref_pop_name,] 
  
  # Iterate through 13 STRs to replace missing or 0 values with imputed values
  for (single_str in strs) { 
    
    # For each STR we will look at each loci individually
    for (i in 1:length(ref_pop_csv[[single_str]])){
      allele_freq = ref_pop_csv[[single_str]][i]
      
      # If an allele frequency is 0, replace is with imputed af value. 
      if(allele_freq == 0.0){
        ref_pop_csv[[single_str]][i] = round(imputed_df[single_str], digits = 4)
      }
    }
  }
  
  ####################Code that adds missing alleles from sim.pop to ref.pop ###################
  
  #This will identifies what alleles are missing in ref.pop (meaning not in afs_csv)
  
  alleles_tobe_added <- setdiff(sim_pop_csv$Allele, ref_pop_csv$Allele)
  
  #creates a vector with nvalues repeated the length of the rows 
  vector_toappend_refpop <- rep(max(imputed_df), length(colnames(ref_pop_csv)))
  
  if (length(alleles_tobe_added) != 0){
    ##this loop appends the ref.pop file to add missing alleles 
    for (allele_frequency in 1:length(alleles_tobe_added)) {
      
      if(alleles_tobe_added[allele_frequency] == 1){
        
      }else{
        vector_toappend_refpop[[1]] <- alleles_tobe_added[allele_frequency]
        
        ref_pop_csv <- rbind(ref_pop_csv, vector_toappend_refpop)
      }
    }
  }  
  # Sort the Alleles based on the numerical order of str counts recorded. 
  ref_pop_csv = ref_pop_csv[order(ref_pop_csv$Allele, decreasing = FALSE), ]
  
  return (ref_pop_csv)
}

#################### END THE CROSS ANALYSIS WITH CORRECTION FUNCTON ################################
LR_calculator_cross <- function(sim_pop, ref_pop, num_contrib, num_sims, is.a.truecontrib){
  strs <- c("CSF1PO","D3S1358","D5S818","D7S820","D8S1179","D13S317","D16S539",
            "D18S51","D21S11","FGA","TH01","TPOX","VWA")
  
  #reads in simulation population allele frequency table
  sim_pop_csv <- read.csv(sim_pop)
  ref_pop_csv <- read.csv(ref_pop)
  
  ref_pop_csv <- correct_zero(sim_pop_csv, ref_pop_csv, ref_pop, strs)
  
  #####tabfreq makes an object that holds the following information###
  # @TAB function reads in all the allele frequencies per loci 
  # @which.loc reports the loci that are taken into consideration
  # @pop.names are right now just reads "population" but will try to change it to the actual population (Changed)
  
  pop.afs <- tabfreq(tab = sim_pop_csv, pop.names = as.factor(sim_pop))
  # print(paste("this is pop.afs:", as.character(pop.afs))) # searching for error
  
  #4/23/20 NC -- aim 3 
  ref_pop_afs <<- tabfreq(tab =  ref_pop_csv, pop.names = as.factor(ref_pop))
  
  # This is where the LRs from each iteration is stored
  truecontrib.LR.vec <- c()
  noncontrib.LR.vector <- c()
  ##############################
  ### true contributor code ####
  ##############################
  
  
  if (is.a.truecontrib == 1){
    
    j = 1
    #While loop loops through value of num_sims
    while (j <= num_sims){
      #popgen$tab.geno gives the genotyprs of all individuals (n)
      
      # Simulate genotypes based on sim pop allele frequency tabs for the number
      # of contributors the user specifies. 
      sim.genotypes <- simugeno(tab = pop.afs, 
                                n = num_contrib, 
                                which.loc = strs)
      # Simulate a mixture using the simulated genotypes from simugeno
      sim.mix <- simumix(sim.genotypes,
                         ncontri = num_contrib) 
      
      #set the loci names in order here instead of at the start NC 10/8/20
      loci.names.inorder = sim.genotypes$which.loc
      # print(paste("These are the loci names in order:", loci.names.inorder)) 
      singleLR_vector <- c()
      
      k = 1 
      while (k < 14){  
        
        known.contrib.all.atk.pros = c()
        for(i in 1:num_contrib){
          known.contrib.all.atk.pros = c(known.contrib.all.atk.pros, 
                                         as.numeric(strsplit(sim.mix$mix.prof[i,k], "/")[[1]])
          )
        } 
        if   (num_contrib == 1){
          known.contrib.all.atk.def = 0
        } else { 
          known.contrib.all.atk.def = c()
          
          for (i in 1:(num_contrib - 1)){
            known.contrib.all.atk.def = c(known.contrib.all.atk.def,
                                          as.numeric(strsplit(sim.mix$mix.prof[i,k], "/")[[1]])
            )
          }
        }
        
        known.noncontrib.all.atk.def <- c()
        known.noncontrib.all.atk.def <-  as.numeric(strsplit(sim.mix$mix.prof[num_contrib, k], "/")[[1]])
        
        ###############################
        ### True contrib Single_LR ####
        ###############################
        
        single_LR_1 <<- list(LR = 5)
        single_LR_2 <<- list(LR = 6)
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001)) {
          
          #print("LRs are different, running again!")
          
          single_LR_1 <<- LR( Repliste = c(as.numeric(sim.mix@mix.all[[k]])),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = rep(0.01, num_contrib),
                              prDHom = rep(0.0001, num_contrib),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq = ref_pop_afs@tab[[1]][[loci.names.inorder[k]]]
          )
          
          single_LR_2 <<- LR( Repliste = c(as.numeric(sim.mix@mix.all[[k]])),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = rep(0.01, num_contrib),
                              prDHom = rep(0.0001, num_contrib),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq = ref_pop_afs@tab[[1]][[loci.names.inorder[k]]]
                              
          )
        }
        singleLR_vector <- c(singleLR_vector, single_LR_1$LR)
        k = k + 1
      }
      
      # Adds the LR for this sim to the log10_LR vector 
      truecontrib.LR.vec[j] <- log10(prod(singleLR_vector))
      j = j + 1
      
    }
    return(truecontrib.LR.vec)
  }
  
  ##########################
  ### non contributors #####
  ##########################
  
  else if (is.a.truecontrib == 0){   
    j = 1
    
    while (j <= num_sims){     
      
      genos.in.mix <- simugeno(tab = pop.afs, 
                               n = num_contrib, 
                               which.loc = strs)
      
      
      sim.mix <- simumix(genos.in.mix,
                         ncontri = num_contrib
      )
      
      
      noncon.sus <- simugeno(tab = pop.afs, 
                             n =  1, 
                             which.loc = strs
      )
      
      loci.names.inorder = genos.in.mix$which.loc
      
      singleLR_vector <- c()
      #############################
      ### non contributors == 1+ ##
      #############################
      
      k = 1
      
      while (k < 14){  
        
        known.contrib.all.atk.pros <- c()
        
        if(num_contrib == 1){
          known.contrib.all.atk.pros = as.numeric(strsplit(noncon.sus$tab.geno[1,k], "/")[[1]])
          
        } else {
          
          for(i in 1:(num_contrib-1)){
            ###looking at alleles at k under the prosecution
            known.contrib.all.atk.pros = c( known.contrib.all.atk.pros, 
                                            as.numeric(strsplit(sim.mix$mix.prof[i,k], "/")[[1]]))
          }
          known.contrib.all.atk.pros = c(known.contrib.all.atk.pros,as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                                                        "/")[[1]]))
        }
        
        known.contrib.all.atk.def = c()
        
        if(num_contrib == 1){
          known.contrib.all.atk.def = 0
        } else { 
          for (i in 1:(num_contrib-1)){
            known.contrib.all.atk.def = c(known.contrib.all.atk.def,
                                          as.numeric(strsplit(sim.mix$mix.prof[i, k], "/")[[1]]))
          }
        }
        
        #this is the known non contributor according to the defense in this case (i.e the suspect)
        known.noncontrib.all.atk.def <- c()
        known.noncontrib.all.atk.def = c(as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                             "/")[[1]]))
        
        #############################
        ### non contrib Single_LR ##
        #############################
        single_LR_1 <<- list(LR = 5)
        single_LR_2 <<- list(LR = 6)
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001)) {
          
          single_LR_1 <<- LR( Repliste = c(as.numeric(sim.mix@mix.all[[k]])),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = rep(0.01, num_contrib),
                              prDHom = rep(0.0001, num_contrib),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              freq = ref_pop_afs@tab[[1]][[loci.names.inorder[k]]]
          )
          
          
          single_LR_2 <<- LR( Repliste = c(as.numeric(sim.mix@mix.all[[k]])),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = rep(0.01, num_contrib),
                              prDHom = rep(0.0001, num_contrib),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              freq = ref_pop_afs@tab[[1]][[loci.names.inorder[k]]])
        }
        single_LR = single_LR_1$LR
        singleLR_vector <- c(singleLR_vector,single_LR)
        k = k + 1
        
      }
      # Take log10 of LR values, this is preformed for thw whole vector
      noncontrib.LR.vector[j] <- log10(prod(singleLR_vector))
      j = j + 1
      #end of else.
      
    }
    #end of while (j < num_sims), which loops over num_sims
    return(noncontrib.LR.vector)
  }
  
  
}

## Main Class, where all the magic begins!! 
args <- commandArgs(trailingOnly = TRUE)

# User input flags 
option_list = list(
  make_option(c("-c", "--ncontrib"), type="numeric", default=NA, 
              help="how many contributors", metavar="character"),
  
  make_option(c("-s", "--nsims"), type="numeric", default=NA, 
              help="how many sims", metavar="character"),
  
  make_option(c("-p", "--pop"), type="numeric", default=NA, 
              help="choose list number", metavar="numeric"),
  
  make_option(c("-t", "--truecontrib"), type = "numeric", default=NA,
              help = "contributor = 1, noncontributor = 0", metavar = "numeric"),
  
  make_option(c("-r", "--rand_seed"), type = "numeric", default = 1,
              help = "sets random seed number for simulation", metavar = "numeric"),
  
  make_option(c("-o", "--out_prefix"), type = "character", 
              help = "sets random seed number for simulation", metavar = "character")
); 

# Output user input to individual variables 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# Unload user inputs into variables
num_contrib = opt$ncontrib
num_sims = opt$nsims
seed = opt$rand_seed
is.a.truecontrib = opt$truecontrib

set.seed(seed) 

#Getting Data
Nvalues <-readRDS("input_data/Nvalues_244pops.Rdata")

# Create refrence population list. 
pops.for.sims = readRDS('pops.for.sims.RDS')
pop_label = pops.for.sims[opt$pop]
pop_fp = paste0('input_data/', pop_label)
pop_label = strsplit(pop_label, split = '.csv')[1]

# RUN LOW TEMPLEATE ANALYSIS
results <- LR_calculator_cross(pop_fp, pop_fp, num_contrib, num_sims, is.a.truecontrib)

# Create filename based on if the simulation is in mix or not. 
if (is.a.truecontrib==1){
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_',num_sims, '_', num_contrib,'_contribINmix.rds')
}else{
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_', num_sims, '_', num_contrib,'_contribNotINmix.rds') 
}

# Output results at RDS file 
saveRDS(results, file = filename)