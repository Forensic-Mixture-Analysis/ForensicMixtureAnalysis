# Direct and Cross Simulation Workflow.


Inside this github directory I will go over how to preform simulations preformed for
the Direct and Cross Analysis used inside the manuscript. [Decreased accuracy of forensic DNA mixture analysis for groups with lower genetic diversity] (https://www.biorxiv.org/content/10.1101/2023.08.25.554311v1)

This pipeline was run on Wynton, a HPC provided by UCSF. All the 
scripts made here are meant to be run on that system and should be modified to 
fit the cluster you are running on. In particular, the shell call used in `run_cross_analysis.sh`
and `run_direct_analysis.sh` uses the prefix xvfb-run Rscript to run our Rscripts 
which is specific to UCSF's super computer. 


## Workflow for Direct Analysis 

Direct simulations where the reference population is the same as the direct population. 

run_all_jobs.sh -> run_direct_analysis.sh -> direct_analysis_arr.R

direct_analysis is designed to run LTA simulations with each population in `informed_consent_groups.csv` 

### run_all_jobs.sh 
Used to submit all jobs for direct and cross scenarios within one command 

```{bash}
bash run_all_jobs.sh
```

### run_direct_analysis.sh
Used to call direct_analysis_arr.R to run LTA simulation analysis for
each subpopuation group. Each subpopulation is ran with its own job. This script
is used for a cluster that submits jobs via qsub and qstat. Users must use
the -t parameter prior to calling the shell script to specify which population simulation's 
you are trying to run. 

```{bash}
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-83 run_direct_analysis.sh -t 0 -c 2 -s 100000
```


### direct_analysis_arr.R 
This is the main R script that preform LTA simulation analysis
using forensim. This rscript should be interacted with the command line. There 
are 6 main parameters the user must enter. For this script only direct simulations will be preformed
where the refrence population is the same as the simulation population.

`-t` - (int) Type of simualtion to preform. 0 - not in mix , 1 - in-mix

`-p` - (int) Population to preform simualtion on, must be between 1-83

`-s` - (int) Number of simualtions to run

`-c` - (int) Number of contributors to run

`-r` - (int) Random seed 

`-o` - (str) output prefix for the results

```{bash}
Rscript direct_analysis_arr.R -t 1 -p 1 -s 10000 -c 1 -r 10 -o pop1_results
```



## Workflow for Cross Analysis 
Cross simulations where the reference population is not the same as the direct population. For a given simulation 
population specified, the simulations will be preformed for each of the 25 other subpopulation used as a reference.
Users will be outputted a 2d matrix, where each row represents the simulation results for a population that was used
as the reference. 

run_all_jobs.sh -> run_cross_analysis.sh -> cross_analysis.sh 

cross_analysis is designed to run LTA simulations with each populations in `trimmed_informed_consent_groups.csv`

### run_all_jobs.sh 
used to submit all jobs for direct and cross scenarios within one command 

```{bash}
bash run_all_jobs.sh
```

### run_cross_analysis.sh
Used to call cross_analysis_arr.R to run LTA simulation analysis for
each subpopuation group. Each subpopulation is ran with its own job. This script
is used for a cluster that submits jobs via qsub and qstat. Users must use
the -t parameter prior to calling the shell script to specify which population simulation's 
you are trying to run. 

```{bash}
qsub -cwd -l h_rt=100:00:00 -l mem_free=8G -t 1-26 run_cross_analysis.sh -t 1 -c 2 -s 10000
```


### cross_analysis_arr.R 
This is the main R script that preform LTA simulation analysis where users can specify the reference popultions to be 
used for LTA simulations. This rscript should be interacted with the command line. There 
are 6 main parameters the user must enter. 

`-t` - (int) Type of simualtion to preform. 0 - not in mix , 1 - in-mix

`-p` - (int) Population to preform simualtion on, must be between 1-26

`-s` - (int) Number of simualtions to run

`-c` - (int) Number of contributors to run

`-r` - (int) Random seed 

`-o` - (str) output prefix for the results

```{bash}
Rscript cross_analysis_arr.R -t 1 -p 1 -s 10000 -c 1 -r 10 -o pop1_results
```

##### Contact
Please Contact Miguel Guardado (Miguel.Guardado @ ucsf.edu) for any questions 
about the simulation framework presented in this directory 

