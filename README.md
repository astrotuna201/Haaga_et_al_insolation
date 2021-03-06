## Forcing of late Pleistocene ice volume by spatially variable summer energy
This repository contains code to reproduce the analyses and figures for Haaga KA, Diego D, Hannisdal B, Brendryen, Forcing of late Pleistocene ice volume by spatially variable summer energy (submitted).

## Instructions 

### System requirements
The easiest way to run the code in this repository is to use RStudio (https://www.rstudio.com/). If you clone or download the repository, an RStudio project file is included. Remember to clear all objects from the workspace in RStudio before you try to re-run any analyses or re-create plots. If you don't use the provided project file, remember to create a new RStudio project in the repository folder on your computer. Otherwise, relative paths in the scripts will not work.

The code has been tested on OSX El Capitan 10.11.6 and Linux Mint 18.2. The code may or may not run on Windows.

### Required packages
All R packages required are loaded automatically when you run any of the scripts in this repository. 

### Data
#### Summer energy
Latitudinal summer energy data was precomputed for a range of summer energy thresholds using the code accompanying *Huybers, Peter. "Early Pleistocene glacial cycles and the integrated summer insolation forcing." Science 313.5786 (2006): 508-511*. These files are located in the `data/Integrated_summer_energy/` folder, and there is one file for each latitude. 

#### Global sea level (GSL)
The GSL data are from *Spratt, Rachel M., and Lorraine E. Lisiecki. "A Late Pleistocene sea level stack." Climate of the Past 12.4 (2016): 1079* and are found in the `data/` folder. Spratt and Lisiecki's age assignments for GSL is based on the LR04 stack, which is tuned to an ice model forced by insolation. Here, we have modified the GSL record to age model with orbitally independent age assignments. There is one `.txt` file in the `data/` folder for each age model. In the manuscript, we focus on the age model with orbitally independent age assignments ('speleoice'). 

### Preprocessing
Loading the data used in the paper is done with the `preprocessing/preprocessing.R` script. Running this generates a data table containing all the necessary data for the CCM analyses, which is saved in the file `analysis/compiled_data`.

For details on the preprocessing, check out the source code for the other files in the `preprocessing/` folder. 

*Note: preprocessing might take several hours, depending on your system specs*.

### Analyses
For the analysis, we use CCM wrappers (around the rEDM package implementation) from the `tstools` repository (https://github.com/kahaaga/tstools) that simplifies lagged CCM analyses with surrogate testing. The `tstools` package also contains functions used to estimate embedding dimensions, and to summarise CCM results with associated significance tests.

All computations have been pre-run and summarised for this repository. Summaries are located in the `results/` folder. To re-run analyses, check the instructions in the `analysis/run_analyses.R` script. 

*Note: for the paper we used a cluster of high-performance computers. Running the analyses on a laptop or workstation computer, depending on your system specs, will take anything from weeks to several months.*

### Figures
Figures can be reproduced by running the individual scripts in the `figures/` folder. Alternatively, you can create all the figures by running the `figures/plot_all_figures.R` script. 

*Note: generating the figures takes a few minutes.*
