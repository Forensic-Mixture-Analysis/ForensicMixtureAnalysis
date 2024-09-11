module load CBI r/4.1

# Input parameters 
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


pop_list=("Berber.1_new.csv" "China.5_new.csv" "Cree.243_source_new.csv" "Eygpt.1_new.csv" "Eygpt.2_new.csv" "FBIAfAm_expanded_new.csv"         
          "FBIApache_expanded_new.csv" "FBICauc_expanded_new.csv" "FBIGuamFilipinos_expanded_new.csv" "FBINavajo_expanded_new.csv" "FBISEHisp_expanded_new.csv"
          "FBISWHisp_expanded_new.csv" "FBITrinidad_expanded_new.csv" "Hungary.3_new.csv" "Macedonia.2_new.csv" "Maldives_new.csv" "NahuaPuebla.240_new.csv" "ParanaBrazil.241_new.csv"         
          "Romani.6_new.csv" "S.Africa.2_new.csv" "Salishan.243_source_new.csv" "San_new.csv" "Spain.4_new.csv" "Tibet.6_new.csv" "Tzeltal.240_new.csv" "WNahua.240_new.csv")

pop=${pop_list[${SGE_TASK_ID}-1]}
echo Running Simulation on population $pop ....

# Make a file directory for this jobs output, jobs 2-54 will throw this as an error but it will move on lmao
output_dir=results/LR_Cross_26_050524_${t}_${s}_${c}
mkdir $output_dir

xvfb-run Rscript cross_analysis_arr.R -t $t -p $pop -s $s -c $c -r 10 -o $output_dir

[[ -n "$JOB_ID" ]] && qstat -j "$JOB_ID"

