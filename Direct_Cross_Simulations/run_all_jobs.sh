# Direct Analysis worst case job submission

## Not in mix
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 1 -c 2 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 1 -c 3 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 1 -c 4 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 1 -c 5 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 1 -c 6 -s 100000

## In mix
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 0 -c 2 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 0 -c 3 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 0 -c 4 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 0 -c 5 -s 100000
qsub -cwd -l h_rt=40:00:00 -l mem_free=8G -t 1-244 run_direct_analysis.sh -t 0 -c 6 -s 100000



# Cross Analysis worse case job submission

## In mix
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 1 -c 2 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 1 -c 3 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 1 -c 4 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 1 -c 5 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 1 -c 6 -s 10000

## Not in mix
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 0 -c 2 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 0 -c 3 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 0 -c 4 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 0 -c 5 -s 10000
qsub -cwd -l h_rt=60:00:00 -l mem_free=8G -t 1-54 run_cross_analysis.sh -t 0 -c 6 -s 10000
