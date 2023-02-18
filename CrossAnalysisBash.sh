#!/bin/bash
for i in 2 3 4 5 6
do
    Rscript CrossAnalysis10k.R -t 1 -l 1 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 2 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 3 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 4 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 5 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 6 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 7 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 8 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 9 -s 10000 -c $i  &
    Rscript CrossAnalysis10k.R -t 1 -l 10 -s 10000 -c $i
done

# removed --save after $i 
# -l = listnum = list of pops 
# - c = num of contribs 
# -s = number of simulations
# -t = contributor = 1 noncontributor = 0 