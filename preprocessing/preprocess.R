################################################################################
# preprocess.R:
# 
# Running this script will generate a file "datasets.RData" in the "data/" f
# folder of this RStudio project. This file contains a 9-column data frame 
# the relevant data  we're using in the paper. 
# 
# This will take anywhere from 30 minutes to several hours to run, depending on 
# the system specs.
################################################################################

# Load the relevant source code. To limit the complexity of the code,
# most of the heavy lifting is done behind the scenes. Feel free to 
# inspect the source code if you like.
source("preprocessing/preprocess_compile_datasets.R")

# Define which summer energy thresholds we're interested in. 
# To calculate summer energy, we sum the daily insolation intensity 
# over days of the year where the diurnal average insolation
# exceeds the threshold value. Here, the summer energy for 
# all thresholds and latitudes are precomputed using the Matlab
# code accompanying Huybers (2006). These files are located in the 
# 
thresholds = seq(0, 500, 25)
latitudes = seq(-90, 90, 1)

# Set the temporal resolution of the data. We'll use 1 kyr for the main
# analyses; this is the same resolution as the sea level (GSL) and temperature
# (GAST) data. Lower resolutions are not considered; they would
# yield interpolation bias (introducing dynamics we have no way of 
# verifying if is real or not).
bin.sizes = c(1)

# Create a list to store all the generated dataframes.
datasets = list()

for (threshold in thresholds) {
  for (bin.size in bin.sizes) {
    for (latitude in latitudes) {
      cat("Threshold: ", threshold, 
          "\t Bin size: ", bin.size, 
          "\t Latitude: ", latitude, "\n")
      # Compile the datasets we're going to use (these are
      # loaded behind the scenes - the only thing we're doing
      # here are putting them on a common age grid with the
      # resolution we specify).
      dt = suppressMessages(CompileGlobalClimateData(threshold = threshold, 
                                                     latitude = latitude,
                                                     bin.size = bin.size))
      
      # Add columns with information about temporal resolution, 
      # threshold and latitude (we need to keep track of this when performing
      # CCM analyses).
      dt$threshold = rep(threshold)
      dt$latitude = rep(latitude)
      dt$bin.size = rep(bin.size)
      
      # Store the compiled data before compiling with different 
      # values of the threshold, latitude and temporal resolution
      datasets[[paste(threshold, latitude, bin.size, sep = ".")]] = dt
    }
  }
}

# Bind dataframes vertically into one data table
dt = data.table::rbindlist(datasets)

# Save to disk, so we don't have to re-create the data later on.
saveRDS(dt, "analysis/compiled_data.RData")
