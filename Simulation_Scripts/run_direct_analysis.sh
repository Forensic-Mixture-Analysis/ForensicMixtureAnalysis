module load CBI r/4.1

while getopts t:s:c: flag
do
    case "${flag}" in
        t) type_sim=${OPTARG};;
        s) sims=${OPTARG};;
        c) cont=${OPTARG};;
    esac
done

# Input parameters 
t=$type_sim
s=$sims
c=$cont

echo $t
echo $s
echo $c

echo Running Simulation on population ${SGE_TASK_ID} ...

# Make a file directory for this jobs output, jobs 2-54 will throw this as an error but it will move on lmao
output_dir=results/LR_Direct_050524_${t}_${s}_${c}
mkdir $output_dir

# xvfb might be static to ucsf-wynton, check Rscript 
xvfb-run Rscript direct_analysis_arr.R -t $t -p ${SGE_TASK_ID} -s $s -c $c -r 10 -o $output_dir

[[ -n "$JOB_ID" ]] && qstat -j "$JOB_ID"
