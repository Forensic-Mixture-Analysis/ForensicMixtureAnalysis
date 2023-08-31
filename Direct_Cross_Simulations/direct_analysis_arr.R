suppressPackageStartupMessages(suppressWarnings(library(tkrplot)))
library(forensim)
library(readr)
library(optparse)

LR_calculator <- function(file.name, num_contrib, num_sims, is.a.truecontrib, seed){
  
  # This sets a seed so we can have reproducible data
  set.seed(seed) 
  
  # MG - Set the STRS 
  strs <- c("CSF1PO","D3S1358","D5S818","D7S820","D8S1179","D13S317","D16S539",
            "D18S51","D21S11","FGA","TH01","TPOX","VWA")
  
  # Reads in the files we are working with 
  afs_csv <- read.csv(file.name)   
  
  
  #####tabfreq makes an onject that holds the following information###
  # @TAB function reads in all the allele frequencies per loci 
  # @which.loc reports the loci that are taken into consderation
  # @pop.names are right now just reads "population" but will try to change it to the actual population (Changed)
  
  pop.afs <- tabfreq(tab = afs_csv, 
                     pop.names = as.factor(file.name)
  )
  
  
  # This is where the LRs from each iteration is stored
  truecontrib.LR.vec <- c()
  noncontrib.LR.vector <- c()
  #APRIL 5, 2020; NC 
  #This makes sure the STRs are in the right order 
  loci.names.inorder = strs
  
  
  ##############################
  ### true contributor code ####
  ##############################
  
  
  if (is.a.truecontrib == 0){
    j = 1 
    
    #I think this should be <=
    
    #***********************LOOK IN HERE FOR SPEEDUP####
    #*
    #*TURN STUFF INSIDE WHILE LOOP INTO ITS OWN FUNCTION
    #*#THEN *APPLY
    #*#FUNCTION WILL TAKE THOSE 
    #*95 GIVE FUNCTION 
    while (j < num_sims){
      
      # PRINT OUT J - add a timestamp before and after *** 
      #print(j)
      #######simugeno objects store genotypes from the tabfreq ###########
      #popgen$tab.geno gives the genotyprs of all individuals (n) 
      sim.genotypes <- simugeno(tab = pop.afs, 
                                n = num_contrib, 
                                which.loc = loci.names.inorder
                                # APRIL 29, 20; NIQUO C
                                # CHANIGING THIS SO THAT THE CODE READS STRS FROM THE SAME VECTOR THROUGHOUT 
                                #c("CSF1PO",
                                #c("CSF1PO",
                                #"D3S1358, "D5S818" "D7S820","D8S1179 "D13S317","D16S539","D18S51","D21S11","FGA", "TH01",
                                # "TPOX",
                                # "VWA"
                                
      )
      #simulate a mixture using the simulated genotypes  
      sim.mix <- simumix(sim.genotypes,
                         ncontri = num_contrib
      ) 
      
      singleLR_vector <- c()
      
      
      ### Definition: pros - prosecution
      ### Definition: def - defense
      
      ## With 1x simulation
      ## k is originally set to 14
      ## k at 14 was ~80-90 seconds
      ## k at 4 was ~25-30 seconds
      ## What is k ?
      
      ##TODO: This while loop is taking up all of the time.
      ### k is an str - 1 to 14 then pops to the next section
      k = 1 
      while (k < 14){  
        
        ### Define as a list filled with starting values
        ### Should have num_contrib genotypes
        known.contrib.all.atk.pros = c()
        
        
        for(i in 1:num_contrib){
          #### TODO: CLEAN UP VARIABLE NAMES
          #### TODO: look at what is in sim.mix and genotype. look for more efficient way to take alleles out
          #### turn loop into a simpler statement
          
          ### Looking at alleles at k under the prosecution
          ### k = 1 then all the way to 13
          
          ### reallocating memory everytime.
          ### TODO: Define the entire array before the loop rather than filling in
          
          
          known.contrib.all.atk.pros = c(known.contrib.all.atk.pros, 
                                         as.numeric(strsplit(sim.mix$mix.prof[i,k], "/")[[1]])
          )
        } 
        
        
        if   (num_contrib == 1){
          known.contrib.all.atk.def = 0
        } else {known.contrib.all.atk.def = c()
        
        ### WHY: num_contrib-1 instead of just num_contrib
        for (i in 1:(num_contrib - 1)){
          #### doing the same thing as above
          ## ACTUALLY NOT TRUE - above is .pros below is .def
          known.contrib.all.atk.def = c(known.contrib.all.atk.def,
                                        as.numeric(strsplit(sim.mix$mix.prof[i, k], "/")[[1]])
          )
        }
        
        
        
        }
        known.noncontrib.all.atk.def <- c()
        known.noncontrib.all.atk.def <-  as.numeric(strsplit(sim.mix$mix.prof[num_contrib, k], "/")[[1]])
        
        ##############################
        ### True contrib Single_LR ####
        ##############################
        single_LR <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                          Tp = c(known.contrib.all.atk.pros),
                          ## Vd = Non contrib under Hd - Suspect ##
                          Td = c(known.contrib.all.atk.def),
                          Vp = 0,
                          ## Vp = Non contrib under Hp - 0 ##
                          Vd = known.noncontrib.all.atk.def, 
                          xd = 1,
                          xp = 0,
                          #### ADJUSTING THETA###
                          theta = 0.00,
                          prDHet = c(0.2,0.2),
                          prDHom = c(0.04,0.04),
                          prC = 0,
                          freq = pop.afs@tab[[1]][[loci.names.inorder[k]]]
        )
        
        
        
        
        
        
        #######################################
        ##### True Contrib Infinity loop #######        
        ######################################       
        
        ###FIX April 6 2021 ###
        
        single_LR_1 <<- list(LR = 5)
        single_LR_2 <<- list(LR = 6)
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001)) {
          
          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0.00,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq = pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              
          )
          
          single_LR_2 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0.00,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq =pop.afs@tab[[1]][[loci.names.inorder[k]]]
          )
        }
        
        ###### FIX AS OF APRIL 6, 2021
        single_LR$LR = single_LR_1$LR
        
        singleLR_vector <- c(singleLR_vector,single_LR$LR)
        
        ## increment k
        k = k + 1
        
        #end of while (k < 14), for LR calculation  
        
        
      }
      
      ##RETURN SOMETHING FROM FUNCTION THEN SAVE IT TO RESULTS truecontrib.LR.vec[]
      #RETURN THIS VALUE - log10(prod(single))
      #SAPPLY should return the vector of one simulation
      
      # Adds the LR for this sim to the log10_LR vector 
      truecontrib.LR.vec[j] <- log10(prod(singleLR_vector))
      j = j + 1
      
      #end of while (j < num_sims)
    }
    
    #end of if (is.a.truecontrib == 0)
  }
  
  ##########################
  ### non contributors #####
  ##########################
  
  else if (is.a.truecontrib == 1){   
    #set.seed (458) 
    j = 1
    
    while (j < num_sims){ 
      
      genos.in.mix <- simugeno(tab = pop.afs, 
                               n = num_contrib, 
                               which.loc = loci.names.inorder
                               # APRIL 29, 20; NIQUO C
                               # CHANIGING THIS SO THAT THE CODE READS STRS FROM THE SAME VECTOR THROUGHOUT 
                               #c("CSF1PO",
                               # c("CSF1PO","D3S135,D5S818","D7S820","D8S1179","D13S317","D16S539","D18S51","D21S11", "FGA","TH01","TPOX", "VWA"
                               
      )
      sim.mix <- simumix(genos.in.mix,
                         ncontri = num_contrib)
      
      
      noncon.sus <- simugeno(tab = pop.afs, 
                             n =  1, 
                             which.loc = loci.names.inorder)
      
      singleLR_vector <- c()
      #############################
      ### non contributors == 1+ ##
      #############################
      
      k = 1
      
      while (k < 14){  
        
        known.contrib.all.atk.pros <- c()
        
        if(num_contrib == 1){
          known.contrib.all.atk.pros = as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                           "/")[[1]])
        } else {
          
          for(i in 1:(num_contrib)){
            known.contrib.all.atk.pros = c( known.contrib.all.atk.pros, 
                                            as.numeric(strsplit(sim.mix$mix.prof[i,k], 
                                                                "/")[[1]])
            )
          }
          known.contrib.all.atk.pros = c(known.contrib.all.atk.pros,as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                                                        "/")[[1]]))
        }
        
        known.contrib.all.atk.def = c()
        
        if(num_contrib == 1){
          known.contrib.all.atk.def = 0
        } else { 
          for (i in 1:(num_contrib)){
            known.contrib.all.atk.def = c(known.contrib.all.atk.def,
                                          as.numeric(strsplit(sim.mix$mix.prof[i, k], "/")[[1]])
            )
          }
        }
        known.noncontrib.all.atk.def <- c()
        known.noncontrib.all.atk.def = c(as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                             "/")[[1]]))
        
        #############################
        ### non contrib Single_LR ##
        #############################
        
        single_LR <- LR( Repliste = c(sim.mix$mix.all[[k]]),
                         Tp = known.contrib.all.atk.pros,
                         Td = known.contrib.all.atk.def,
                         Vp = 0,
                         #Vd = known.noncontrib is noncon.sus
                         Vd = known.noncontrib.all.atk.def,
                         xd = 1,
                         xp = 0,
                         theta = 0.00,
                         prDHet = c(0.2,0.2),
                         prDHom = c(0.04,0.04),
                         prC = 0,
                         freq = pop.afs@tab[[1]][[loci.names.inorder[k]]]
        )
        
        single_LR_1 <<- list(LR = 5)
        single_LR_2 <<- list(LR = 6)
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001)) {
          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0.00,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              freq =pop.afs@tab[[1]][[loci.names.inorder[k]]])
          
          single_LR_2 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0.00,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              freq =pop.afs@tab[[1]][[loci.names.inorder[k]]])
        }
        single_LR$LR = single_LR_1$LR
        
        singleLR_vector <- c(singleLR_vector, single_LR$LR)
        k = k + 1
      }

      noncontrib.LR.vector[j] <- log10(prod(singleLR_vector))
      j = j + 1
      #end of else, for more than 1 contributor
    }
    return(noncontrib.LR.vector)
  }
  return(truecontrib.LR.vec)
}
#### END OF FUNCTION ##########################################################
######################################################################


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


#Getting Data
Nvalues <-readRDS("input_data/Nvalues_244pops.Rdata")

# Create refrence population list. 
pops.for.sims = readRDS('pops.for.sims.RDS')
pop_label = pops.for.sims[opt$pop]
pop_fp = paste0('input_data/', pop_label)
pop_label = strsplit(pop_label, split = '.csv')[1]

# RUN LOW TEMPLEATE ANALYSIS
results <- LR_calculator(pop_fp, num_contrib, num_sims, is.a.truecontrib, seed)

# Create filename based on if the simulation is in mix or not. 
if (is.a.truecontrib==0){
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_',num_sims, '_', num_contrib,'_contribINmix.rds')
}else{
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_', num_sims, '_', num_contrib,'_contribNotINmix.rds') 
}

# Output results at RDS file 
saveRDS(results, file = filename)


result <- c()

for (pop in pops.for.sims){
  pop_fp = paste0('input_data/', pop)
  res <- LR_calculator(pop_fp, 6, 2, 0, 1000*100000)
  result <- c(result, res)
}
print(result)