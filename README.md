## Forcing of late Pleistocene ice volume by spatially variable summer energy
This repository contains code to reproduce the analyses and figures for Haaga KA, Diego D, Hannisdal B, Brendryen, Forcing of late Pleistocene ice volume by spatially variable summer energy (submitted).

## Instructions 
### Preprocessing
Loading the data used in the paper is done with the `preprocessing/preprocessing.R` script. Running this generates a data table containing all the necessary data for the CCM analyses. For details on the preprocessing, check out the source code for the other files in the `preprocessing/` folder. *Note: this might take several hours, depending on your system specs*.

### Analyses
For the analysis, we use CCM wrappers (around the rEDM package implementation) from the `tstools` repository (https://github.com/kahaaga/tstools). This package also contains functions used to estimate embedding dimensions, and to summarise the CCM results.  

All computations have been pre-run and summarised for this repository. To re-run analyses, check the instructions in the `analysis/run_analyses.R` script (note: a cluster of high-performance computers were used; running the analyses on a laptop or workstation computer, depending on your system specs, will take anything from weeks to several months).

### Figures
Figures can be reproduced by running the individual scripts in the `figures/` folder. Alternatively, you can create all the figures by running the `figures/plot_all_figures.R` script.
