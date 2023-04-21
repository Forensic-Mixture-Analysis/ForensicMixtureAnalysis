###########################
#### START OF FUNCTION ####
###########################
######################### set working directory ###############################
#setwd("C:/Users/evanh/Dropbox/PC/Documents/FST Lab Work/LTA.244.pops")
#####################################################################################
# LOAD LIBRARY PACKAGES 
suppressPackageStartupMessages(suppressWarnings(library(tkrplot)))
library(forensim)
library(readr)
#################### BUILD THE CROSS ANALYSIS FUNCTON ################################
LR_calculator_cross <- function(simulation_population, ref.pop,num_contrib, num_sims, is.a.truecontrib){
  
  strs <- c(        "CSF1PO",
                    "D3S1358",
                    "D5S818",
                    "D7S820",
                    "D8S1179",
                    "D13S317",
                    "D16S539",
                    "D18S51",
                    "D21S11",
                    "FGA",
                    "TH01",
                    "TPOX",
                    "VWA"
  )
 # print(paste("These are the strs:", strs)) # using as a marker to see where the error occurs
  # We have to use this particular seed for reproduceability 
  #set.seed (123560) 
  
  #reads in simulation population allele frequency table
  afs_csv <- read.csv(simulation_population)

  
  ########## adding in a loop that replaces zeros with nvalues  ###################
  
  ref.pop.csv_1 <- read.csv(ref.pop)   #reads in the reference population allele frequency table
  
  currentpop_zero_sub <-Nvalues[ref.pop,] # N_values for the reference population 
  
  
  for (locus in 1:length(strs)) { # this loop looks at only the 13 str loci we will use to simulate people 
    
    locus= strs[locus] 
#    print(paste("This is the locus:", locus)) # view strs of x 
    
    
    for(locus_name in 1:length(colnames(ref.pop.csv_1))){ #connecting loci.names.in order to the colnames in the ref.pop.csv file
      
      
      if(colnames(ref.pop.csv_1[locus_name]) == locus){ #we will change n values ONLY for STRS in Loci.names in order 
        
        
        for (ref_allele_freq in 1:length(ref.pop.csv_1[[locus]])){
          
          allele_freq <-ref.pop.csv_1[[locus]][[ref_allele_freq]]
#          print(allele_freq)
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
    while (j <= num_sims){
      #print(j)
      #######simugeno objects store genotypes from the tabfreq ###########
      #popgen$tab.geno gives the genotyprs of all individuals (n) 
      sim.genotypes <- simugeno(tab = pop.afs, 
                                n = num_contrib, 
                                which.loc =strs  #the strs need to be changed in all the code NC 10/8/20
                                
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
        
        single_LR_1 <<- list(LR = 5)
        
        single_LR_2 <<- list(LR = 6)
        
        #print(single_LR_1$LR)
        #print(single_LR_2$LR)
        
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 || 
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               single_LR_1$LR != single_LR_2$LR) {
          
          #print("LRs are different, running again!")
          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              
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
                              theta = 0,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              
          )
        }
        single_LR = single_LR_1$LR
        #debugging - NC 06/4/20 
        #print(single_LR$LR)
        
        
        singleLR_vector <- c(singleLR_vector,single_LR)
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
    
    while (j < num_sims){     
      
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
        
        
        single_LR_1 <<- list(LR = 5)
        
        single_LR_2 <<- list(LR = 6)
        
        while (is.na(single_LR_1$LR) || is.infinite(single_LR_1$LR) || is.nan(single_LR_1$LR) || single_LR_1$LR<0 ||
               is.na(single_LR_2$LR) || is.infinite(single_LR_2$LR) || is.nan(single_LR_2$LR) || single_LR_2$LR<0 ||
               signif(single_LR_1$LR, 5) != signif(single_LR_2$LR, 5)) {
          
          #print(paste("IN WHILELOOP!", single_LR_1$LR, single_LR_2$LR))
          
          
          single_LR_1 <<- LR( Repliste = c(sim.mix@mix.all[[k]]),
                              Tp = c(known.contrib.all.atk.pros),
                              ##Vd = Non contrib under Hd - Suspect ##
                              Td = c(known.contrib.all.atk.def),
                              Vp = 0,
                              ## Vp = Non contrib under Hp - 0 ##
                              Vd = known.noncontrib.all.atk.def,
                              xd = 1,
                              xp = 0,
                              theta = 0,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              
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
                              theta = 0,
                              prDHet = c(0.2,0.2),
                              prDHom = c(0.04,0.04),
                              prC = 0,
                              # Aim 3- this uses the af's of the reference population
                              #pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              #freq forensim adds in 0.000239808153477218"
                              #forloop here
                              freq =ref.pop.afs@tab[[1]][[loci.names.inorder[k]]]
                              
          )
          
        }
        
        single_LR = single_LR_1$LR
        
        
        singleLR_vector <- c(singleLR_vector,single_LR)
        k = k + 1
        
        
        #end of  while (k < 14)  
      }
      # debugging 2/20 Niquo - I added log10
      #noncontrib.LR.vector[j] <- prod(singleLR_vector)
      
      noncontrib.LR.vector[j] <- log10(prod(singleLR_vector))
      j = j + 1
      #end of else, for more than 1 contributor 
      
    }
    
    #end of while (j < num_sims), which loops over num_sims
    #this is where the return statement goes for this loop 
    return(noncontrib.LR.vector)
  }
  
  return(truecontrib.LR.vec)
  
}


#### END OF FUNCTION ################################################################################################################################

args <- commandArgs(trailingOnly = TRUE)
#print(args)

library("optparse")

option_list = list(
  make_option(c("-c", "--ncontrib"), type="numeric", default=NA, 
              help="how many contributors", metavar="character"),
  make_option(c("-s", "--nsims"), type="numeric", default=NA, 
              help="how many sims", metavar="character"),
  make_option(c("-l", "--listnum"), type="numeric", default=NA, 
              help="choose list number", metavar="character"),
  make_option(c("-t", "--truecontrib"), type = "numeric", default= NA,
                help = "contributor = 1, noncontributor = 0", metavar = "numeric")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

num_contrib = opt$ncontrib
num_sims = opt$nsims
listnum = opt$listnum
is.a.truecontrib = opt$truecontrib

###LR_calculator_cross(simulation_population, ref.pop,num_contrib, num_sims, is.a.truecontrib)

Nvalues <-readRDS("Nvalues_244pops.Rdata")
#is.a.truecontrib = 1 # 1 = contributor 0 != contributor

#####################################################################################################################################################################################################

#Getting Data
NonContribSimulation <- function(simtest){
 # print(paste("At the beginning of the simulation the val of simtest is:", simtest))
  if (simtest == 1) {
    simtest = c("Angola.1_new.csv","Aust.Abor.2_new.csv","CearaBrazil.232_new.csv","China.10_new.csv","China.11_new.csv")
  } else if (simtest == 2){ 
    simtest = c("Cree.243_source_new.csv","Derung.249_new.csv","E.Slovak.5_new.csv","ETimor.229_new.csv","Eygpt.2_new.csv")
  } else if (simtest == 3){
    simtest = c("FBIAfAm_expanded_new.csv","FBICauc_expanded_new.csv","FBISWHisp_expanded_new.csv","FBITrinidad_expanded_new.csv")
  } else if (simtest == 4){
    simtest = c("FBIApache_expanded_new.csv", "FBISEHisp_expanded_new.csv", "FBIGuamFilipinos_expanded_new.csv","FBINavajo_expanded_new.csv")
  } else if (simtest == 5){ 
    simtest = c("FujianChina.233_new.csv","India.16_new.csv","India.2_new.csv","Inuit.1_new.csv","Inuit.3_source_new.csv","Malaysia.2_new.csv","Malaysia.4_new.csv")
  } else if (simtest == 6){
    simtest = c("Maldives_new.csv","Mazatex.240_new.csv","Mexico.6_new.csv","NahuaChapala.240_new.csv","Nat.Am.10_new.csv")
  } else if (simtest == 7){
    
    simtest = c("Nat.Am.13_new.csv","Nat.Am.14_new.csv","Nat.Am.15_new.csv","Nat.Am.16_new.csv","Nat.Am.4_new.csv","Nat.Am.8_new.csv")
  } else if (simtest == 8){
    
    simtest = c("Nat.Am.9_source_new.csv","Nepal_new.csv","Nu.249_new.csv","NZE.Polynesian_new.csv","NZW.Polynesian_new.csv","Ojbwe.243_new.csv")
  } else if (simtest == 9){
    
    simtest = c("Romani.5_new.csv","RutheneVoj_new.csv","SAColoured.242_new.csv","Salishan.243_source_new.csv","San_new.csv","Tibet.10_new.csv")
  } else {
    simtest = c('Tibet.5_new.csv', 'Tibet.8_new.csv', 'Tibet.9_new.csv', 'Tojolabal.240_new.csv', 'Tzotzil.240_new.csv', 'Uganda_new.csv')
  }
  
  
  set.seed(1109)
  
  
  pops.for.sims =  c("Angola.1_new.csv","Aust.Abor.2_new.csv","CearaBrazil.232_new.csv","China.10_new.csv","China.11_new.csv",
                     "Cree.243_source_new.csv","Derung.249_new.csv","E.Slovak.5_new.csv","ETimor.229_new.csv","Eygpt.2_new.csv",
                     "FBIAfAm_expanded_new.csv","FBICauc_expanded_new.csv","FBISWHisp_expanded_new.csv","FBITrinidad_expanded_new.csv",
                     "FBIApache_expanded_new.csv", "FBISEHisp_expanded_new.csv", "FBIGuamFilipinos_expanded_new.csv","FBINavajo_expanded_new.csv",
                     "FujianChina.233_new.csv","India.16_new.csv","India.2_new.csv","Inuit.1_new.csv","Inuit.3_source_new.csv","Malaysia.2_new.csv","Malaysia.4_new.csv",
                     "Maldives_new.csv","Mazatex.240_new.csv","Mexico.6_new.csv","NahuaChapala.240_new.csv","Nat.Am.10_new.csv",
                     "Nat.Am.13_new.csv","Nat.Am.14_new.csv","Nat.Am.15_new.csv","Nat.Am.16_new.csv","Nat.Am.4_new.csv","Nat.Am.8_new.csv",
                     "Nat.Am.9_source_new.csv","Nepal_new.csv","Nu.249_new.csv","NZE.Polynesian_new.csv","NZW.Polynesian_new.csv","Ojbwe.243_new.csv",
                     "Romani.5_new.csv","RutheneVoj_new.csv","SAColoured.242_new.csv","Salishan.243_source_new.csv","San_new.csv","Tibet.10_new.csv",
                     "Tibet.5_new.csv","Tibet.8_new.csv","Tibet.9_new.csv","Tojolabal.240_new.csv","Tzotzil.240_new.csv","Uganda_new.csv")
  

  
  for (sim_pop in simtest) {
    cross_pairs <- c()
    list_of_results <- c()
    for (ref.pop in pops.for.sims) {
      if (sim_pop != ref.pop) {
        simulation_population = sim_pop
        results <- LR_calculator_cross(simulation_population, ref.pop,num_contrib, num_sims, is.a.truecontrib)
        cross_pairs <- c(cross_pairs,paste(sim_pop, ref.pop))
        list_of_results <- c(list_of_results, list(results))
      }
    }
    resultsdict <- vector(mode="list", length= length(cross_pairs))
    names(resultsdict) <- cross_pairs
    r = 1
    for (result in list_of_results) {
      resultsdict[[r]] <- result
      r = r + 1
    }
    popname = sim_pop
   # print(popname)
    position = unlist(gregexpr("_", popname))
    x = position[1] - 1
    filename = substr(popname, 1, x)
    filename2 = gsub(' ','', paste('LRCross_',filename,'_10k_',num_contrib,'contribINmix.rds'))
    saveRDS(resultsdict, file = filename2)
  }
}
#print(paste("This is the val before we call the fx:", listnum))
NonContribSimulation(listnum) # calls the function and runs it 

#pop1 <- readRDS("LRCross_Tibet.5_1k_2contribmix.rds")
#pop1 <- as.data.frame(pop1)

#n = readRDS("NValues_244pops.Rdata")
