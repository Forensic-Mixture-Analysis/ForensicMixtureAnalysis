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


pop_list=("Angola.1_new.csv" "Aust.Abor.2_new.csv" "CearaBrazil.232_new.csv" "China.10_new.csv" "China.11_new.csv" "Cree.243_source_new.csv" "Derung.249_new.csv" "E.Slovak.5_new.csv" "ETimor.229_new.csv" "Eygpt.2_new.csv"
           "FBIAfAm_expanded_new.csv" "FBICauc_expanded_new.csv" "FBISWHisp_expanded_new.csv" "FBITrinidad_expanded_new.csv"
           "FBIApache_expanded_new.csv" "FBISEHisp_expanded_new.csv" "FBIGuamFilipinos_expanded_new.csv" "FBINavajo_expanded_new.csv"
           "FujianChina.233_new.csv" "India.16_new.csv" "India.2_new.csv" "Inuit.1_new.csv" "Inuit.3_source_new.csv" "Malaysia.2_new.csv" "Malaysia.4_new.csv"
           "Maldives_new.csv" "Mazatex.240_new.csv" "Mexico.6_new.csv" "NahuaChapala.240_new.csv" "Nat.Am.10_new.csv"
           "Nat.Am.13_new.csv" "Nat.Am.14_new.csv" "Nat.Am.15_new.csv" "Nat.Am.16_new.csv" "Nat.Am.4_new.csv" "Nat.Am.8_new.csv"
           "Nat.Am.9_source_new.csv" "Nepal_new.csv" "Nu.249_new.csv" "NZE.Polynesian_new.csv" "NZW.Polynesian_new.csv" "Ojbwe.243_new.csv"
           "Romani.5_new.csv" "RutheneVoj_new.csv" "SAColoured.242_new.csv" "Salishan.243_source_new.csv" "San_new.csv" "Tibet.10_new.csv"
           "Tibet.5_new.csv" "Tibet.8_new.csv" "Tibet.9_new.csv" "Tojolabal.240_new.csv" "Tzotzil.240_new.csv" "Uganda_new.csv")

pop=${pop_list[${SGE_TASK_ID}-1]}
echo Running Simulation on population $pop ....

# Make a file directory for this jobs output, jobs 2-54 will throw this as an error but it will move on lmao
output_dir=results/LR_Cross_${t}_${s}_${c}
mkdir $output_dir

# xvfb is specific syntax to run on ucsf's wynton HPC. 
xvfb-run Rscript LR_cross_calc_wc.R -t $t -p $pop -s $s -c $c -r 77 -o $output_dir


