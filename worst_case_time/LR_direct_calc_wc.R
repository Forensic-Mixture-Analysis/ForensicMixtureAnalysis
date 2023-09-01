suppressPackageStartupMessages(suppressWarnings(library(tkrplot)))
library(forensim)
library(readr)
library(optparse)

LR_calculator_cross_worse <- function(simulation_population, ref.pop, num_contrib, num_sims, is.a.truecontrib, seed){
  
  
  # Set seed of the simulation
  set.seed(seed)
  
  ## Establish order of STR to use
  strs <- c("CSF1PO","D3S1358","D5S818","D7S820","D8S1179","D13S317","D16S539",
            "D18S51","D21S11","FGA","TH01","TPOX","VWA")
  

  
  #reads in simulation population allele frequency table
  afs_csv <- read.csv(simulation_population)
  
  
  ########## adding in a loop that replaces zeros with nvalues  ###################
  
  ref.pop.csv_1 <- read.csv(ref.pop)   #reads in the reference population allele frequency table
  ref_pop_name = as.list(strsplit(ref.pop, split = '/'))[[1]][2]
  currentpop_zero_sub <-Nvalues[ref_pop_name,] # N_values for the reference population 
  
  
  for (locus in 1:length(strs)) { # this loop looks at only the 13 str loci we will use to simulate people
    
    locus= strs[locus]
    
    #    print(paste("This is the locus:", locus)) # view strs of x
    for(locus_name in 1:length(colnames(ref.pop.csv_1))){ #connecting loci.names.in order to the colnames in the ref.pop.csv file
      
      if(colnames(ref.pop.csv_1[locus_name]) == locus){ #we will change n values ONLY for STRS in Loci.names in order
        
        for (ref_allele_freq in 1:length(ref.pop.csv_1[[locus]])){
          allele_freq <-ref.pop.csv_1[[locus]][[ref_allele_freq]]
          
          if(allele_freq == 0.0){
            ref.pop.csv_1[[locus]][ref_allele_freq] <-currentpop_zero_sub[[locus]]
            
          }
        }
      }
    }
  }
  
  ################# end of loop that replaces 0 af's with 5/2N values ########################
  
  #################### Start of code that adds missing  alleles to ref.pop ###################
  
  #this loop identifies what alleles are missing in ref.pop (meaning not in afs_csv)
  alleles_tobe_added <- c(1)
  for (allele_frequency in 1:length(afs_csv[[1]])) {
    if (afs_csv[[1]][[allele_frequency]] %in% ref.pop.csv_1[[1]]){
    } else{
      alleles_tobe_added <- c(alleles_tobe_added, afs_csv[[1]][[allele_frequency]])
    }
    
  }
  
  #print(alleles_tobe_added)
  #appending ref.pop.csv
  ref.pop.csv <- ref.pop.csv_1
  
  #nvalue that is going to assigned to new rows -- CSF1PO is arbritrary
  nvalue_sub <- currentpop_zero_sub$CSF1PO
  
  #creates a vector with nvalues repeated the length of the rows
  vector_toappend_refpop <- rep(nvalue_sub, length(colnames(ref.pop.csv)))
  
  ##this loop appends the ref.pop file to add missing alleles
  
  for (allele_frequency in 1:length(alleles_tobe_added)) {
    
    if(alleles_tobe_added[allele_frequency] == 1){
      #print(paste("empty_list_ofalleles"))
    }else{
      #print(paste("in_else_statement"))
      #changes the value of the first postion to the allele that is missing
      vector_toappend_refpop[[1]] <- alleles_tobe_added[allele_frequency]
      
      #binds the new data to ref.pop.csv
      ref.pop.csv <- rbind(ref.pop.csv, vector_toappend_refpop)
      
    }
    
  }
  
  #this checks that the missing alleles have indeed been added
  check<- c()
  for (allele_frequency in 1:length(afs_csv[[1]])) {
    if (afs_csv[[1]][[allele_frequency]] %in% ref.pop.csv[[1]]){
      
    } else{
      # print(afs_csv[[1]][[allele_frequency]])
      check <<- c(check, afs_csv[[1]][[allele_frequency]])
      # print(paste("alleles are missing from the refrence population"))
    }
    
  }
  something_diff <- ref.pop.csv
  #print(something_diff)
  #################### end the code that adds alleles to ref.pop ##################################
  
  
  
  #####tabfreq makes an onject that holds the following information###
  # @TAB function reads in all the allele frequencies per loci 
  # @which.loc reports the loci that are taken into consderation
  # @pop.names are right now just reads "population" but will try to change it to the actual population (Changed)
  
  pop.afs <- tabfreq(tab = afs_csv, 
                     pop.names = as.factor(simulation_population)
  )
  # print(paste("this is pop.afs:", as.character(pop.afs))) # searching for error
  
  #4/23/20 NC -- aim 3 
  ref.pop.afs <<- tabfreq(tab =  something_diff, 
                          pop.names = as.factor(ref.pop)
  )
  
  # This is where the LRs from each iteration is stored
  truecontrib.LR.vec <- c()
  noncontrib.LR.vector <- c()
  
  
  #Testing seed
  #set.seed (123560) 
  
  
  
  ##############################
  ### true contributor code ####
  ##############################
  
  
  if (is.a.truecontrib == 1){
    
    j = 1
    
    #While loop loops through value of num_sims
    while (j < num_sims){
      #print(j)
      #######simugeno objects store genotypes from the tabfreq ###########
      #popgen$tab.geno gives the genotyprs of all individuals (n) 
      sim.genotypes <- simugeno(tab = pop.afs, 
                                n = num_contrib, 
                                which.loc = strs  #the strs need to be changed in all the code NC 10/8/20
                                
      )
      
      #simulate a mixture using the simulated genotypes  
      sim.mix <- simumix(sim.genotypes,
                         ncontri = num_contrib
                         
      ) 
      
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
        } else {known.contrib.all.atk.def = c()
        for (i in 1:(num_contrib - 1)){
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
        
        ############################### JUST ADDED 2/22/21 ########################################################################################                
        
        single_LR_1 <<- list(LR = 1)
        single_LR_2 <<- list(LR = 2)

        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001) ) {

          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.noncontrib.all.atk.def), ## Just the suspects alleles + unique
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = 0, ## Remove this
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = c(known.noncontrib.all.atk.def),
                              xd = num_contrib,
                              xp = num_contrib-1,
                              theta = 0,
                              prDHet = c(0.2, 0.2),
                              prDHom = c(0.04, 0.04),
                              prC = 0,
                              #  Aim 3- this uses the af's of the reference population
                              #  pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #  freq forensim adds in 0.000239808153477218"
                              #  forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]])
          
          single_LR_2 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.noncontrib.all.atk.def), ## Just the suspects alleles + unique
                              #  Vd = Non contrib under Hd - Suspect ##
                              Td = 0, ## Remove this
                              Vp = 0,
                              #  Vp = Non contrib under Hp - 0 ##
                              Vd = c(known.noncontrib.all.atk.def),
                              xd = num_contrib,
                              xp = num_contrib-1,
                              theta = 0,
                              prDHet = c(0.2, 0.2),
                              prDHom = c(0.04, 0.04),
                              prC = 0,
                              #  Aim 3- this uses the af's of the reference population
                              #  pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #  freq forensim adds in 0.000239808153477218"
                              #  forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]])

        }
        single_LR = single_LR_1$LR
        print(single_LR)
        
        singleLR_vector <- c(singleLR_vector, single_LR)
        k = k + 1

        #end of while (k < 14), for LR calculation   
      }
      
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
  
  else if (is.a.truecontrib == 0){   
    #set.seed (458) 
    j = 1
    noncon.geno <- c()
    genosinmix <- c()
    
    while (j <= num_sims){     
      
      genos.in.mix <- simugeno(tab = pop.afs, 
                               n = num_contrib, 
                               which.loc = strs
                               
                               
      )
      #Added by Cara to keep track of sim mixture genotypes and case
      genosinmix <- c(genosinmix, list(genos.in.mix$tab.geno))
      
      sim.mix <- simumix(genos.in.mix,
                         ncontri = num_contrib
      )
      
      
      
      
      noncon.sus <- simugeno(tab = pop.afs, 
                             n =  1, 
                             which.loc = strs
                             
                             
      )
      #Added by Cara to keep track of non-contributor genotypes and case
      noncon.geno <- c(noncon.geno, list(noncon.sus$tab.geno))
      
      loci.names.inorder = genos.in.mix$which.loc
      
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
            
            ###looking at alleles at k under the prosecution
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
          # debugging: Niquo 2/19/20 (i in 1:(num_contribs - 1))
          for (i in 1:(num_contrib)){
            known.contrib.all.atk.def = c(known.contrib.all.atk.def,
                                          as.numeric(strsplit(sim.mix$mix.prof[i, k], "/")[[1]])
            )
          }
        }
        
        #this is the known non contributor according to the defense in this case (i.e the suspect)
        known.noncontrib.all.atk.def <- c()
        known.noncontrib.all.atk.def = c(as.numeric(strsplit(noncon.sus$tab.geno[1,k], 
                                                             "/")[[1]]))
        
        #############################
        ### non contrib Single_LR ##
        #############################
        
        ###Added by Cara 3/1
        
        
        single_LR_1 <<- list(LR = 1)
        single_LR_2 <<- list(LR = 2)
        
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 ||
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               (abs(single_LR_1$LR - single_LR_2$LR) > .001)) {
          
          # print(paste("IN WHILELOOP!", single_LR_1$LR, single_LR_2$LR))
          # print(k)
          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                               Tp = c(known.noncontrib.all.atk.def), ## Just the suspects alleles + unique
                               ##Vd = Non contrib under Hd - Suspect ##
                               Td = 0, ## Remove this
                               Vp = 0,
                               ## Vp = Non contrib under Hp - 0 ##
                               Vd = c(known.noncontrib.all.atk.def),
                               xd = num_contrib, # 6 cont
                               xp = num_contrib-1,
                               theta = 0,
                               prDHet = c(0.2, 0.2),
                               prDHom = c(0.04, 0.04),
                               prC = 0,
                               #  Aim 3- this uses the af's of the reference population
                               #  pop.afs@tab[[1]][[loci.names.inorder[k]]]
                               #  freq forensim adds in 0.000239808153477218"
                               #  forloop here
                               freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]])
          # print(single_LR_1$LR)

          single_LR_2 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                               Tp = c(known.noncontrib.all.atk.def), ## Just the suspects alleles + unique
                               #  Vd = Non contrib under Hd - Suspect ##
                               Td = 0, ## Remove this
                               Vp = 0,
                               #  Vp = Non contrib under Hp - 0 ##
                               Vd = c(known.noncontrib.all.atk.def),
                               xd = num_contrib,
                               xp = num_contrib-1,
                               theta = 0,
                               prDHet = c(0.2, 0.2),
                               prDHom = c(0.04, 0.04),
                               prC = 0,
                               #  Aim 3- this uses the af's of the reference population
                               #  pop.afs@tab[[1]][[loci.names.inorder[k]]]
                               #  freq forensim adds in 0.000239808153477218"
                               #  forloop here
                               freq=ref.pop.afs@tab[[1]][[loci.names.inorder[k]]])
        }
        single_LR = single_LR_1$LR
        print(single_LR)
        
        
        singleLR_vector <- c(singleLR_vector,single_LR)
        
        k = k + 1
        
        
        #end of  while (k < 14)  
      }
      # debugging 2/20 Niquo - I added log10

      noncontrib.LR.vector[j] <- log10(prod(singleLR_vector))
      j = j + 1
    }
    
    #end of while (j < num_sims), which loops over num_sims
    #this is where the return statement goes for this loop 
    return(noncontrib.LR.vector)
  }
  
  return(truecontrib.LR.vec)
  
}


## Main Class, where all the magic begins!! 
start = Sys.time()

args <- commandArgs(trailingOnly = TRUE)

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


num_contrib = opt$ncontrib
num_sims = opt$nsims
seed = opt$rand_seed
is.a.truecontrib = opt$truecontrib


#Getting Data
Nvalues <-readRDS("input_data/Nvalues_244pops.Rdata")


# Refrence population list. 
pops.for.sims <- readRDS('pops.for.sims.RDS')
pop_label = pops.for.sims[opt$pop]
print(pop_label)
pop_fp = paste0('input_data/', pop_label)

results <- LR_calculator_cross_worse(pop_fp, pop_fp, num_contrib, num_sims, is.a.truecontrib, seed)

pop_label = strsplit(pop_label, split = '.csv')[1]

if (is.a.truecontrib==1){
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_',num_sims, '_', num_contrib,'_contribINmix.rds')
  
  time_filename = paste0('results/time_results/LRCross_', pop_label, '_seed_', 
                         opt$rand_seed,'_',num_sims, '_', num_contrib,'_contribINmix.rds')
  
}else{
  filename = paste0(opt$out_prefix, '/LRDirect_', pop_label, '_seed_', 
                    opt$rand_seed,'_', num_sims, '_', num_contrib,'_contribNotINmix.rds')
  
  time_filename = paste0('results/time_results/LRCross_', pop_label, '_seed_', 
                         opt$rand_seed,'_',num_sims, '_', num_contrib,'_contribNotINmix.rds')
  
}
saveRDS(results, file = filename)

## Save Total Runtime it took to run this simulation
end = Sys.time()
total_time = as.numeric(end-start)
saveRDS(total_time, file = time_filename)
