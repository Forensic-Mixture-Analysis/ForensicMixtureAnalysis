library(readr)


loci_vec <- c("CSF1PO","D3S1358","D5S818","D7S820",
              "D8S1179","D13S317","D16S539","D18S51","D21S11",
              "FGA","TH01","TPOX","VWA")

population_files <- list.files(pattern="*new.csv")

sum_to_1 = character() 
range_1.00001 = character()
range_1.0001 = character()
range_1.001 = character()
range_1.01 = character()
range_1.1 = character()

for (a_file in population_files) {          #takes in a file from population_files
  counter = 0   #Counter that will count amount of columns
  pop_file <- read_csv(a_file)    #open the population file
  for (i in 2:length(colnames(pop_file))) {  #for each column in the list of column names given with the colnames() method
    if (colnames(pop_file[i]) %in% loci_vec== TRUE){   #if the names from the colnames() list  are in the loci_vec, previously made
      sum_column = sum(pop_file[i])   #sum_column is a variable for the value of the sum of the column
      if (sum_column == 1) {  #finding how many columns have AF between two values or at 1
        counter = counter + 1     #Counter adds one when it has value
      }
    }
  }
  if (counter == 13 ) {       #This can be set to however. If the counter is greater than zero, meaning there is a column
    sum_to_1 <- append(sum_to_1, a_file)
  }
}







#print(paste('population file is', a_file, 'locus is', colnames(pop_file[i]), 'AF sum is', sum_column))



for (i in population_files) {
  if (i %in% sum_to_1) { #List can be changed
    NA
  } else {
    print(i)
  }
} 
  

