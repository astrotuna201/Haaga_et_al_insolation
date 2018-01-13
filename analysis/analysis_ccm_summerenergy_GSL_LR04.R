# This script performs CCM between summer energy and GSL on the LR04 age model.

# We cross map from GSL (the putative response) to summer energy (the 
# putative driver). The reasoning for this is that if summer energy at 
# a particular latitude was a dynamical influence on GSL, then GSL should 
# contain information about that summer energy forcing. If it does, then we 
# should be able to cross map that summer energy time series from GSL.
# If this cross maping is successful (and passes the tests outlined in the
# manuscript), we interpret this as a causal dynamical forcing of GSL by 
# summer energy.
#
# ----------------------------------------------------------
# Analysis information (read this before running the script)
# ----------------------------------------------------------
# One CCM analysis is performed for each latitude-threshold configuration, 
# and each result is stored as an .RData file in the 
# results/crossmap_summerenergy_GSL_speleoice folder.  
# 
# NB! Each result file is around 21 Mb when using the analysis setup outline 
# in the paper. For a total of ~3400 files, that amounts to almost 90 Gb. 
# This script was run on a cluster of 4 machines with 6 core 3,5 Ghz Intel Xeon 
# processors with 64 Gb memory each. On the cluster, analyses took around three 
# weeks to finish. Running the script laptop or workstation with lesser 
# configurations may take significantly longer.
# 
# A lightweight version of the analysis can be run by lowering the number of 
# samples and/or the number of surrogates used for each analysis.
# 
# Precomputed summaries are located in the 
# 'results/crossmap_summerenergy_GSL_speleoice_summaries' folder of this 
# project.
# 


########
# SETUP
########
# Install the 'tstools' package. It contains the latest wrappers of the CCM 
# routine from Sugihara et al. (2012), as well as functions to work with the 
# CCM results. The  package also handles everything related to embedding 
# dimension estimation and surrogate data generation. For details and 
# references, see the package documentation for 'tstools'.
devtools::install_github("kahaaga/tstools")

# Load the tstools package and other necessary packages.
require(tstools)
require(dplyr)
require(data.table)
require(pracma)

#########################
# LOAD PRECOMPILED DATA
#########################
# Read the data (must have been precomputed by running the 
# "function/preprocess.R" script).
summer.energy.data <- readRDS("analysis/compiled_data.RData")

# Get the fraction of zero values in the summer energy time series (these
# must have been precomputed using the 'fraction_of_zeros.R' script).
fraction.of.zeros <- readRDS("analysis/fraction_of_zeros.RData") %>% 
  as.data.frame


#########################
# CCM analysis parameters
#########################
# Symmetric lags of 8 kyr are necessary for results after the lagtest to 
# converge. The corresponding maximum lag for the age model with orbitally
# independent age assignments is 8 kyr. This difference possibly reflects the 
# build-in lag after insolation in the LR04 age model.
lags <- -8:8  
E <- NULL # Leaving embedding dimension as NULL triggers optimisation.
tau <- NULL # Set maximum embedding lag as 1 to not loose any data.
library.sizes <- 50
lib <- c(1, dim(data)[1])
pred <- lib 
samples.original <- 20
samples.surrogates <- 20
n.surrogates <- 20
surrogate.methods <- c("aaft")
time.unit <- NULL
time.bin.size <- NULL
num.neighbours <- E + 1
random.libs <- TRUE
with.replacement <- TRUE
exclusion.radius <- 30 
epsilon <- NULL
RNGseed <- 1111
silent <- TRUE
time.run <- F
print.to.console <- T
time.series.length.threshold <- 100
library_column <- 1
target_column <- 2
surrogate_column <- target_column
convergence.test <- TRUE
parallel <- TRUE
parallelize.on.each.lag <- F
num.cores <- parallel::detectCores() - 1
regression.convergence.plots <- F
always.run.surrogates <- F
n.libsizes.convergence.check <- 20
optimise.FNNdim <- T
optimise.boxcountdim <- T
min.E <- 2 
max.E <- 10
min.tau <- 1 
max.tau <- 1
plot.simplex.projection <- F


#########################
# Perform CCM analysis
#########################
ccm <- list()

thresholds <- seq(0, 500, 25)
latitudes <- seq(-90, -90, 1)
bin.sizes <- c(1)

for (l in latitudes) {
  for (b in bin.sizes) {
    for (t in thresholds) {
      cat("latitude: ", l, "\tbin.size: ", b, "\tthreshold: ", t, "\n")
      frac.zeros <- subset(x = fraction.of.zeros, 
                           subset = latitude == l & threshold == t)$value[1]
      # Subset the data
      if (frac.zeros > 0.02) {
        too.many.zeros <- TRUE
        cat("Too many zeros for latitude = ", l, 
            " and threshold = ", t, ".\n\n")
      } else {
        pracma::tic(gcFirst = T)
        
        # Subset data for this latitude and threshold
        df <- subset(x = summer.energy.data,
                     subset = (threshold == t & latitude == l),
                     select = c("SummerEnergy", "GSL")) %>% 
          as.data.frame
        
        # Cross-map
        ccm.gslspel <- tstools::ccm_lagged(
          data = df, 
          lags = lags,
          library.sizes = library.sizes,
          samples.original = samples.original,
          samples.surrogates = samples.surrogates,
          surrogate.methods = surrogate.methods,
          n.surrogates = n.surrogates,
          exclusion.radius = exclusion.radius,
          library.column = "GSL", # The GSL column is GSL on the LR04 age model
          target.column = "SummerEnergy",
          surrogate.column = "SummerEnergy",
          optimise.FNNdim = optimise.FNNdim,
          optimise.boxcountdim = optimise.boxcountdim,
          parallel = parallel)
        
        ccm.gslspel$latitude <- rep(l)
        ccm.gslspel$bin.size <- rep(b)
        ccm.gslspel$threshold <- rep(t)
        
        df$threshold <- rep(t)
        filename <- paste("SummerEnergyDrivesGSL_LR04_", 
                          toString(t), "_", toString(l), ".RData", sep = "")
        
        path <- paste("results/crossmap_summerenergy_GSL_LR04/", 
                      filename, sep = "")
        saveRDS(ccm.gslspel, path)
        
        print(Sys.time())
        print(head(ccm.gslspel))
        cat("\n")
        pracma::toc(echo = T)
        cat("\n\n")
      }
    }
  }
}
