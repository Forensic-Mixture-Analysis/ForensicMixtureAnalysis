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

# Input parameters 
t=1
s=1000
c=2


# Make a file directory for this jobs output, jobs 2-54 will throw this as an error but it will move on lmao
output_dir=results/LR_Cross_${t}_${s}_${c}
mkdir $output_dir

# xvfb is specific syntax to run on ucsf's wynton HPC. 
xvfb-run Rscript LR_direct_calc_wc.R -t $t -p ${SGE_TASK_ID} -s $s -c $c -r 10 -o $output_dir
