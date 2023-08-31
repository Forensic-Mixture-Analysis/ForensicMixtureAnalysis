# Direct and Cross Simulation Workflow.


Inside this github directory I will go over how to preform simulations preformed for
the Direct and Cross Analysis used inside the manuscript, [Decreased accuracy of forensic DNA mixture analysis for groups with lower genetic diversity](https://www.biorxiv.org/content/10.1101/2023.08.25.554311v1)

This pipeline was run on Wynton, a HPC provided by UCSF. All the 
scripts made here are meant to be run on that system and should be modified to 
fit the system it is running on. In particular, the shell call used in `run_cross_analysis.sh`
and `run_direct_analysis.sh` uses the prefix xvfb-run Rscript to run our Rscripts 
which is specific to UCSF's super computer. 

## Simulation Overview
For both the Direct and Cross Analysis, simulations were ran for 2-6 contributions
both for events where the suspect is and is not in the mix. 

For direct analysis 100,000 simulations were ran the 244 populations found
in pops.for.sims.RDS file. Each of these 244 simulations has a .csv file found 
inside `input_data` directory. That is used to initialize the allele frequencies 
used for the genetic simulations inside the forensim package. While we handle the
file management of finding these files, to help mitigate the computational challenge of
running 1000000 simulations, we ran each population in parallel, 
submitting each population as an individual job using the 244 populations as its index representation in `pops.for.sims`

`run_all_jobs.sh`, `run_cross_analysis.sh`, `run_direct_analysis.sh`

These three scripts handle the bash calling of the two R scripts used to run
each `cross_analysis_arr.R` and `direct_analysis_arr.R`. run_all_jobs.sh will
run all simulations preformed on wynton. Most of these scripts are coded to run
on wynton. We recommend you interact with our simulations using Rscript on the command line. 

### `direct_analysis_arr.R`

Example call to run a simulation to run a 100 simulations for an 2 contributor Angola population 
where the suspect is inside the mixture. Random seed of 5. 

```{r}
Rscript direct_analysis_arr.R -t 0 -p 1 -s 100 -c 2 -r 5 -o $output_dir
```

- `-t` : Type of Simulation to run, Contributor in mix = 0, Contributor not in mix = 1. 
- `-p` : Population to use, index representation of the population is used here. (1-244)
- `-s` : Number of simulations to run.
- `-c` : Number of contributors to run.
- `-r` : Random Seed to Initialize

### `cross_analysis_arr.R` 

Example call to run a simulation to run a 100 simulations for an 2 contributor Angola population 
where the suspect is inside the mixture. Random seed of 5. 

```{r}
Rscript cross_analysis_arr.R -t 1 -p 'Angola.1_new.csv' -s 100 -c 2 -r 5 -o results
```

- `-t` : Type of Simulation to run, Contributor in mix = 1, Contributor not in mix = 0. 
- `-p` : Population to use, string representation of the population is used here. 
- `-s` : Number of simulations to run.
- `-c` : Number of contributors to run.
- `-r` : Random Seed to Initialize.

** The order for in mix/ not in mix simulations are the inverse of each other between in mix and not in mix. Sorry for any confusion **

##### Contact
Please Contact Miguel Guardado (Miguel.Guardado @ ucsf.edu) for any questions 
about the simulation framework presented in this directory 

