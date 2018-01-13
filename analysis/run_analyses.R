# You will need to generate the following subfolders for this script to run 
# successfully:
# * results/crossmap_summerenergy_GSL_LR04
# * results/crossmap_summerenergy_GSL_speleoice.
# 
# CCM results will be written to these directories. 
# 
# NOTE: depending on your system specs, running the complete analyses as 
# specified in the paper will take anything from weeks to months. The analyses
# in the paper were run on a cluster of high-performance computers, and took
# in total around 2 months to finish. See individual scripts for instructions 
# on how to run a lightweight analysis (will still take many days to finish
# for reasonable a reasonable number of samples and number of surrogates).

###############
# CCM ANALYSES
###############
source("analysis/analysis_find_nonzero_configurations.R")
source("analysis/analysis_ccm_summerenergy_GSL_LR04.R")
source("analysis/analysis_ccm_summerenergy_GSL_speleoice.R")

########################
# SUMMARISE CCM ANALYSES
########################
source("analysis/analysis_summarise_crossmapping_summerenergy_GSL_LR04.R")
source("analysis/analysis_summarise_crossmapping_summerenergy_GSL_LR04.R")

###################################
# COMPUTATIONS REQUIRED FOR FIGURES
###################################
source("analysis/analysis_orbital_frequencies.R")
