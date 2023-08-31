# Direct and Cross Simulation Workflow.


Inside this github directory I will go over how to preform simulations preformed for
the Direct and Cross Analysis used inside the manuscript. [Decreased accuracy of forensic DNA mixture analysis for groups with lower genetic diversity] (https://www.biorxiv.org/content/10.1101/2023.08.25.554311v1)

This pipeline was run on Wynton, a HPC provided by UCSF. All the 
scripts made here are meant to be run on that system and should be modified to 
fit the system it is running on. In particular, the shell call used in `run_cross_analysis.sh`
and `run_direct_analysis.sh` uses the prefix xvfb-run Rscript to run our Rscripts 
which is specific to UCSF's super computer. 







##### Contact
Please Contact Miguel Guardado (Miguel.Guardado @ ucsf.edu) for any questions 
about the simulation framework presented in this directory 

